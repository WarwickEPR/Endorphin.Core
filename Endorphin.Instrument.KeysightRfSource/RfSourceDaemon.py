import sys
import zmq
import time
import visa
from random import shuffle

import App

def enum(**enums):
    return type('Enum', (), enums)

Modes = enum(CW=1, RampSweep=2, StepSweep=3, StochasticSweep=4)
ModulationShape = enum(Sine="SINE", Triangle="TRI", Square="SQU")

class RfSourceDaemon:
    def __init__(self, cxt):
        self._interval = 0.002
        self._t0 = 0.0
        self._isquitting = False

        self._mode = Modes.CW
        self._starttime = None
        self._output = None
        self._sweeppoints = 0
        self._dwelltime = None
        self._sweeptime = None
        self._repeatsweep = False
        self._frequencies = None
        self._cwfrequency = None
        self._frequencystart = 0.
        self._frequencystop = 0.
        self._amplitudelimit = None

        self._context = cxt

        self._visaaddr = App.Config()["instrument"]["rfsource"]["visaaddress"]
        print "Attempting to connect to VISA instrument at address [%s...]" % self._visaaddr
        self._v = visa.instrument(self._visaaddr)
        self._v.clear()
        print "Successfully connected to VISA instrument."

        self._id = self._v.ask("*IDN?")
        if not self._id.startswith("Agilent Technologies, E8257D"):
            raise Exception(App.Errors()["instrument"]["rfsource"]["incorrectid"] % {
                "visaaddress": self._visaaddr, "deviceid": self._id})

        self._cmdport = App.Config()["instrument"]["rfsource"]["cmdport"]
        self._logcmds = App.Config()["instrument"]["rfsource"]["logcmds"]
        print "Binding localhost TCP command socket to port [%s]..." % self._cmdport
        self._cmdsocket = self._context.socket(zmq.REP)
        self._cmdsocket.setsockopt(zmq.LINGER, 0)
        self._cmdsocket.bind("tcp://*:%s" % self._cmdport)
        print "Succesfully bound command socket."

        self._cmdhandlers = {
            "ping": self.ping,
            "sett0": self.sett0,
            "quit": self.quit,
            "output": self.output,
            "setoutput": self.setOutput,
            "amplitudelimit": self.amplitudeLimit,
            "setamplitudelimit": self.setAmplitudeLimit,
            "cwamplitude": self.cwAmplitude,
            "setcwamplitude": self.setCwAmplitude,
            "cwfrequency": self.cwFrequency,
            "setcwfrequency": self.setCwFrequency,
            "prepfrequencyrampsweep": self.prepFrequencyRampSweep,
            "prepfrequencystepsweep": self.prepFrequencyStepSweep,
            "startfrequencysweep": self.startFrequencySweep,
            "stopfrequencysweep": self.stopFrequencySweep,
            "instantaneousfrequency": self.instantaneousFrequency,
            "stopmodulation": self.stopModulation,
            "setfrequencymodulation": self.setFrequencyModulation,
            "setamplitudemodulation": self.setAmplitudeModulation
        }

    def run(self):
        print "Entering run loop: waiting for commands."
        try:
            while not self._isquitting:
                loopstart = time.time()
                isempty = True
                message = None
                try:
                    message = self._cmdsocket.recv_pyobj(zmq.DONTWAIT)
                    if self._logcmds:
                        print "Received message %s" % message
                    isempty = False
                except zmq.ZMQError:
                    pass

                if not isempty:
                    if "cmd" not in message:
                        resp = self.response(message, App.Errors()["instrument"]["rfsource"]["nocmd"])
                    elif message["cmd"] not in self._cmdhandlers:
                        resp = self.response(message, App.Errors()["instrument"]["rfsource}"]["unknowncmd"])
                    else:
                        resp = self._cmdhandlers[message["cmd"]](message)
                    self._cmdsocket.send_pyobj(resp)
                    if self._logcmds:
                        print "Responding to message %s [response: %s]" % (message, resp)

                loopend = time.time()
                if (loopend - loopstart) < self._interval:
                    time.sleep(self._interval - (loopend - loopstart))

            print "RF source daemon leaving run method..."

        except Exception as e:
            print "RF source daemon leaving run method after error [%s]" % str(e)

    @staticmethod
    def response(message, error=None, **kwargs):
        resp = {"cmd": None if "cmd" not in message else message["cmd"],
                "error": error}

        if kwargs is not None:
            for key, value in kwargs.iteritems():
                resp[key] = value

        return resp

    @staticmethod
    def hasRequiredParams(message, required):
        for param in required:
            if param not in message:
                return False
        return True

    def missingPararmResponse(self, message, required):
        return self.response(message,
                             App.Errors()["instrument"]["rfsource"]["missingparam"] %
                             (self._cmdport, str(message), str(required)))

    def ping(self, message):
        return self.response(message, pong=True)

    def sett0(self, message):
        if not self.hasRequiredParams(message, ["t0"]):
            return self.missingPararmResponse(message, ["t0"])

        self._t0 = message["t0"]
        return self.response(message)

    def quit(self, message):
        self._isquitting = True
        return self.response(message)

    def output(self, message):
        self._output = bool(self._v.ask("OUTP:STAT?") == "1")
        return self.response(message, output=self._output)

    def setOutput(self, message):
        if not self.hasRequiredParams(message, ["output"]):
            return self.missingPararmResponse(message, ["output"])

        self._output = message["output"]
        self._v.write("OUTP:STAT %s" % ("1" if self._output else "0"))
        return self.response(message)

    def amplitudeLimit(self, message):
        return self.response(message, amplitudelimit=self._amplitudelimit)

    def setAmplitudeLimit(self, message):
        if not self.hasRequiredParams(message, ["amplitudelimit"]):
            return self.missingPararmResponse(message, ["amplitudelimit"])

        self._amplitudelimit = message["amplitudelimit"]
        self._v.write("UNIT:POW DBM")
        if float(self._v.ask("POW?")) > self._amplitudelimit:
            self._v.write("POW %+.2f" % self._amplitudelimit)

        return self.response(message)

    def cwAmplitude(self, message):
        self._v.write("UNIT:POW DBM")
        cwamplitude = float(self._v.ask("POW?"))

        return self.response(message, cwamplitude=cwamplitude)

    def setCwAmplitude(self, message):
        if not self.hasRequiredParams(message, ["cwamplitude"]):
            return self.missingPararmResponse(message, ["cwamplitude"])

        if self._amplitudelimit and message["cwamplitude"] > self._amplitudelimit:
            return self.response(message, App.Errors()["instrument"]["rfsource"]["amplitudelimitexceeded"])

        self._v.write("POW %+.2E" % message["cwamplitude"])
        return self.response(message)

    def cwFrequency(self, message):
        cwfrequency = float(self._v.ask("FREQ:CW?"))
        return self.response(message, cwfrequency=cwfrequency)

    def setCwFrequency(self, message):
        if not self.hasRequiredParams(message, ["cwfrequency"]):
            return self.missingPararmResponse(message, ["cwfrequency"])

        self._mode = Modes.CW
        self._starttime = None
        self._cwfrequency = message["cwfrequency"]
        self._v.write("FREQ:MODE CW")
        self._v.write("FREQ:CW %(cwfrequency)+.12E" % message)
        return self.response(message)

    def prepFrequencyRampSweep(self, message):
        if not self.hasRequiredParams(message, ["frequencystart", "frequencystop"]):
            return self.missingPararmResponse(message, ["frequencystart", "frequencystop"])

        self._mode = Modes.RampSweep
        self._starttime = None
        self._frequencystart = message["frequencystart"]
        self._frequencystop = message["frequencystop"]
        self._v.write("OUTP:STAT 0")
        self._v.write("FREQ:MODE SWE")
        self._v.write("SWE:GEN ANAL")
        self._v.write("FREQ:STAR %(frequencystart)+.12E" % message)
        self._v.write("FREQ:STOP %(frequencystop)+.12E" % message)

        if "time" in message:
            self._v.write("SWE:TIME:AUTO 0")
            self._v.write("SWE:TIME %(time).2f" % message)
            self._sweeptime = message["time"]
        else:
            self._v.write("SWE:TIME:AUTO 1")
            self._sweeptime = float(self._v.ask("SWE:TIME?"))

        return self.response(message)

    def stochasticFrequencyList(self, frequencystart, frequencystop, points):
        step = (float(frequencystop) - float(frequencystart)) / (points - 1)
        frequencies = []
        value = frequencystart
        for i in range(points):
            frequencies.append(round(value, 2))
            value += step

        shuffle(frequencies)
        return frequencies

    def prepFrequencyStepSweep(self, message):
        if not self.hasRequiredParams(message, ["frequencystart", "frequencystop", "points", "dwell", "stochastic"]):
            return self.missingPararmResponse(message, ["frequencystart", "frequencystop", "points", "dwell",
                                                        "stochastic"])

        self._starttime = None
        self._dwelltime = message["dwell"]
        self._sweeppoints = message["points"]
        self._sweeptime = self._sweeppoints * self._dwelltime
        if message["stochastic"]:
            self._mode = Modes.StochasticSweep
            self._v.write("OUTP:STAT 0")
            self._v.write("SWE:DWEL %(dwell)+.3E" % message)
            self._v.write("LIST:DWEL:TYPE STEP")
            self._v.write("LIST:TYPE LIST")
            self._v.write("FREQ:MODE LIST")

            self._frequencies = self.stochasticFrequencyList(message["frequencystart"], message["frequencystop"],
                                                             message["points"])
            self._v.write("LIST:FREQ %s" % str.join(",", map(lambda f: "%+.12E" % f, self._frequencies)))
        else:
            self._mode = Modes.StepSweep
            self._frequencystart = message["frequencystart"]
            self._frequencystop = message["frequencystop"]
            self._v.write("OUTP:STAT 0")
            self._v.write("FREQ:STAR %(frequencystart)+.12E" % message)
            self._v.write("FREQ:STOP %(frequencystop)+.12E" % message)
            self._v.write("SWE:POIN %(points)d" % message)
            self._v.write("SWE:DWEL %(dwell)+.3E" % message)
            self._v.write("SWE:GEN STEP")
            self._v.write("LIST:TYPE STEP")
            self._v.write("FREQ:MODE SWE")

        return self.response(message)

    def startFrequencySweep(self, message):
        if not self.hasRequiredParams(message, ["repeat"]):
            return self.missingPararmResponse(message, ["repeat"])

        self._v.write("INIT:CONT 0")
        self._v.write("ABOR")

        self._v.write("INIT;OUTP:STAT 1")
        self._starttime = time.time() - self._t0
        if message["repeat"]:
            self._v.write("INIT:CONT 1")

        self._output = True
        self._repeatsweep = message["repeat"]

        return self.response(message, starttime=self._starttime)

    def stopFrequencySweep(self, message):
        self._starttime = None
        self._output = False

        self._v.write("OUTP:STAT 0")
        self._v.write("INIT:CONT 0")
        self._v.write("ABOR")

        return self.response(message)

    def instantaneousFrequency(self, message):
        if "time" in message:
            reftime = message["time"]
        else:
            reftime = time.time() - self._t0

        frequency = 0.
        timetonextpoint = None
        timefromlastpoint = None

        if self._mode == Modes.CW:
            if self._output is None:
                self._output = self._v.ask("OUTP:STAT?") == "1"

            if self._output:
                if self._cwfrequency is None:
                    self._cwfrequency = float(self._v.ask("FREQ:CW?"))
                frequency = self._cwfrequency

        else:
            if self._starttime:
                timesincestart = reftime - self._starttime
                if self._repeatsweep:
                    timesincestart -= (timesincestart // self._sweeptime) * self._sweeptime
            else:
                timesincestart = None

            if self._mode == Modes.RampSweep:
                if timesincestart is not None and timesincestart < self._sweeptime:
                    frequency = round((timesincestart / self._sweeptime) * (self._frequencystop - self._frequencystart)
                                      + self._frequencystart, 2)
                elif timesincestart is not None:
                    frequency = self._frequencystop

            elif self._mode == Modes.StepSweep:
                if timesincestart is not None and timesincestart < self._sweeptime:
                    point = int(timesincestart / self._dwelltime)
                    timefromlastpoint = timesincestart - point * self._dwelltime
                    timetonextpoint = timefromlastpoint - self._dwelltime
                    step = (self._frequencystop - self._frequencystart) / (self._sweeppoints - 1)
                    frequency = round(point * step + self._frequencystart, 2)
                elif timesincestart is not None:
                    frequency = self._frequencystop

            elif self._mode == Modes.StochasticSweep:
                point = int(timesincestart / self._dwelltime)
                timefromlastpoint = timesincestart - point * self._dwelltime
                timetonextpoint = timefromlastpoint - self._dwelltime
                if point < len(self._frequencies):
                    frequency = self._frequencies[point]
                else:
                    frequency = self._frequencies[-1]

        return self.response(message, frequency=frequency, time=reftime, timefromlastpoint=timefromlastpoint,
                             timetonextpoint=timetonextpoint)

    def stopModulation(self, message):
        self._v.write("OUTP:MOD 0")
        return self.response(message)

    def setFrequencyModulation(self, message):
        if not self.hasRequiredParams(message, ["modulationfrequency", "deviation"]):
            return self.missingPararmResponse(message, ["modulationfrequency", "deviation"])

        if "shape" in message:
            modulationshpae = message["shape"]
        else:
            modulationshpae = ModulationShape.Sine

        self._v.write("AM:STAT 0")
        self._v.write("FM:SOUR INT")
        self._v.write("FM:DEV %(deviation).1f" % message)
        self._v.write("FM:INT:FREQ %(modulationfrequency).1f" % message)
        self._v.write("FM:INT:FUNC:SHAP %s" % modulationshpae)
        self._v.write("FM:STAT 1")
        self._v.write("OUTP:MOD 1")

        return self.response(message)

    def setAmplitudeModulation(self, message):
        if not self.hasRequiredParams(message, ["modulationfrequency", "depth"]):
            return self.missingPararmResponse(message, ["modulationfrequency", "depth"])

        if "shape" in message:
            modulationshape = message["shape"]
        else:
            modulationshape = ModulationShape.Sine

        self._v.write("FM:STAT 0")
        self._v.write("AM:SOUR INT")
        self._v.write("AM:DEPT %(depth).1fPCT" % message)
        self._v.write("AM:INT:FREQ %(modulationfrequency).1f" % message)
        self._v.write("AM:INT:FUNC:SHAP %s" % modulationshape)
        self._v.write("AM:STAT 1")
        self._v.write("OUTP:MOD 1")

        return self.response(message)

class RfSourceDummyDaemon:
    def __init__(self, cxt):
        self._interval = 0.002
        self._t0 = 0.0
        self._isquitting = False

        self._mode = Modes.CW
        self._starttime = None
        self._output = False
        self._sweeppoints = 0
        self._dwelltime = None
        self._sweeptime = None
        self._repeatsweep = False
        self._frequencies = None
        self._cwfrequency = 10e6
        self._frequencystart = 0.
        self._frequencystop = 0.
        self._amplitudelimit = None
        self._cwamplitude = -10

        self._context = cxt

        self._cmdport = App.Config()["instrument"]["rfsource"]["cmdport"]
        self._logcmds = App.Config()["instrument"]["rfsource"]["logcmds"]
        print "Binding localhost TCP command socket to port [%s]..." % self._cmdport
        self._cmdsocket = self._context.socket(zmq.REP)
        self._cmdsocket.setsockopt(zmq.LINGER, 0)
        self._cmdsocket.bind("tcp://*:%s" % self._cmdport)
        print "Succesfully bound command socket."

        self._cmdhandlers = {
            "ping": self.ping,
            "sett0": self.sett0,
            "quit": self.quit,
            "output": self.output,
            "setoutput": self.setOutput,
            "amplitudelimit": self.amplitudeLimit,
            "setamplitudelimit": self.setAmplitudeLimit,
            "cwamplitude": self.cwAmplitude,
            "setcwamplitude": self.setCwAmplitude,
            "cwfrequency": self.cwFrequency,
            "setcwfrequency": self.setCwFrequency,
            "prepfrequencyrampsweep": self.prepFrequencyRampSweep,
            "prepfrequencystepsweep": self.prepFrequencyStepSweep,
            "startfrequencysweep": self.startFrequencySweep,
            "stopfrequencysweep": self.stopFrequencySweep,
            "instantaneousfrequency": self.instantaneousFrequency,
            "stopmodulation": self.stopModulation,
            "setfrequencymodulation": self.setFrequencyModulation,
            "setamplitudemodulation": self.setAmplitudeModulation
        }

    def run(self):
        print "Entering run loop: waiting for commands."
        try:
            while not self._isquitting:
                loopstart = time.time()
                isempty = True
                message = None
                try:
                    message = self._cmdsocket.recv_pyobj(zmq.DONTWAIT)
                    if self._logcmds:
                        print "Received message %s" % message
                    isempty = False
                except zmq.ZMQError:
                    pass

                if not isempty:
                    if "cmd" not in message:
                        resp = self.response(message, App.Errors()["instrument"]["rfsource"]["nocmd"])
                    elif message["cmd"] not in self._cmdhandlers:
                        resp = self.response(message, App.Errors()["instrument"]["rfsource}"]["unknowncmd"])
                    else:
                        resp = self._cmdhandlers[message["cmd"]](message)
                    self._cmdsocket.send_pyobj(resp)
                    if self._logcmds:
                        print "Responding to message %s [response: %s]" % (message, resp)

                loopend = time.time()
                if (loopend - loopstart) < self._interval:
                    time.sleep(self._interval - (loopend - loopstart))

            print "RF source daemon leaving run method..."

        except Exception as e:
            print "RF source daemon leaving run method after error [%s]" % str(e)

    @staticmethod
    def response(message, error=None, **kwargs):
        resp = {"cmd": None if "cmd" not in message else message["cmd"],
                "error": error}

        if kwargs is not None:
            for key, value in kwargs.iteritems():
                resp[key] = value

        return resp

    @staticmethod
    def hasRequiredParams(message, required):
        for param in required:
            if param not in message:
                return False
        return True

    def missingPararmResponse(self, message, required):
        return self.response(message,
                             App.Errors()["instrument"]["rfsource"]["missingparam"] %
                             (self._cmdport, str(message), str(required)))

    def ping(self, message):
        return self.response(message, pong=True)

    def sett0(self, message):
        if not self.hasRequiredParams(message, ["t0"]):
            return self.missingPararmResponse(message, ["t0"])

        self._t0 = message["t0"]
        return self.response(message)

    def quit(self, message):
        self._isquitting = True
        return self.response(message)

    def output(self, message):
        return self.response(message, output=self._output)

    def setOutput(self, message):
        if not self.hasRequiredParams(message, ["output"]):
            return self.missingPararmResponse(message, ["output"])

        self._output = message["output"]
        return self.response(message)

    def amplitudeLimit(self, message):
        return self.response(message, amplitudelimit=self._amplitudelimit)

    def setAmplitudeLimit(self, message):
        if not self.hasRequiredParams(message, ["amplitudelimit"]):
            return self.missingPararmResponse(message, ["amplitudelimit"])

        self._amplitudelimit = message["amplitudelimit"]
        if self._cwamplitude > self._amplitudelimit:
            self._cwamplitude = self._amplitudelimit

        return self.response(message)

    def cwAmplitude(self, message):
        return self.response(message, cwamplitude=self._cwamplitude)

    def setCwAmplitude(self, message):
        if not self.hasRequiredParams(message, ["cwamplitude"]):
            return self.missingPararmResponse(message, ["cwamplitude"])

        if self._amplitudelimit and message["cwamplitude"] > self._amplitudelimit:
            return self.response(message, App.Errors()["instrument"]["rfsource"]["amplitudelimitexceeded"])

        self._cwamplitude = message["cwamplitude"]
        return self.response(message)

    def cwFrequency(self, message):
        return self.response(message, cwfrequency=self._cwfrequency)

    def setCwFrequency(self, message):
        if not self.hasRequiredParams(message, ["cwfrequency"]):
            return self.missingPararmResponse(message, ["cwfrequency"])

        self._mode = Modes.CW
        self._starttime = None
        self._cwfrequency = message["cwfrequency"]
        return self.response(message)

    def prepFrequencyRampSweep(self, message):
        if not self.hasRequiredParams(message, ["frequencystart", "frequencystop"]):
            return self.missingPararmResponse(message, ["frequencystart", "frequencystop"])

        self._mode = Modes.RampSweep
        self._starttime = None
        self._frequencystart = message["frequencystart"]
        self._frequencystop = message["frequencystop"]

        if "time" in message:
            self._sweeptime = message["time"]
        else:
            self._sweeptime = 0.1

        return self.response(message)

    def stochasticFrequencyList(self, frequencystart, frequencystop, points):
        step = (float(frequencystop) - float(frequencystart)) / (points - 1)
        frequencies = []
        value = frequencystart
        for i in range(points):
            frequencies.append(round(value, 2))
            value += step

        shuffle(frequencies)
        return frequencies

    def prepFrequencyStepSweep(self, message):
        if not self.hasRequiredParams(message, ["frequencystart", "frequencystop", "points", "dwell", "stochastic"]):
            return self.missingPararmResponse(message, ["frequencystart", "frequencystop", "points", "dwell",
                                                        "stochastic"])

        self._starttime = None
        self._dwelltime = message["dwell"]
        self._sweeppoints = message["points"]
        self._sweeptime = self._sweeppoints * self._dwelltime
        if message["stochastic"]:
            self._mode = Modes.StochasticSweep

            self._frequencies = self.stochasticFrequencyList(message["frequencystart"], message["frequencystop"],
                                                             message["points"])
        else:
            self._mode = Modes.StepSweep
            self._frequencystart = message["frequencystart"]
            self._frequencystop = message["frequencystop"]

        return self.response(message)

    def startFrequencySweep(self, message):
        if not self.hasRequiredParams(message, ["repeat"]):
            return self.missingPararmResponse(message, ["repeat"])

        self._starttime = time.time() - self._t0
        self._output = True
        self._repeatsweep = message["repeat"]

        return self.response(message, starttime=self._starttime)

    def stopFrequencySweep(self, message):
        self._starttime = None
        self._output = False

        return self.response(message)

    def instantaneousFrequency(self, message):
        if "time" in message:
            reftime = message["time"]
        else:
            reftime = time.time() - self._t0

        frequency = 0.
        timetonextpoint = None
        timefromlastpoint = None

        if self._mode == Modes.CW:
            if self._output:
                frequency = self._cwfrequency
        else:
            if self._starttime:
                timesincestart = reftime - self._starttime
                if self._repeatsweep:
                    timesincestart -= (timesincestart // self._sweeptime) * self._sweeptime
            else:
                timesincestart = None

            if self._mode == Modes.RampSweep:
                if timesincestart is not None and timesincestart < self._sweeptime:
                    frequency = round((timesincestart / self._sweeptime) * (self._frequencystop - self._frequencystart)
                                      + self._frequencystart, 2)
                elif timesincestart is not None:
                    frequency = self._frequencystop

            elif self._mode == Modes.StepSweep:
                if timesincestart is not None and timesincestart < self._sweeptime:
                    point = int(timesincestart / self._dwelltime)
                    timefromlastpoint = timesincestart - point * self._dwelltime
                    timetonextpoint = timefromlastpoint - self._dwelltime
                    step = (self._frequencystop - self._frequencystart) / (self._sweeppoints - 1)
                    frequency = round(point * step + self._frequencystart, 2)
                elif timesincestart is not None:
                    frequency = self._frequencystop

            elif self._mode == Modes.StochasticSweep:
                point = int(timesincestart / self._dwelltime)
                timefromlastpoint = timesincestart - point * self._dwelltime
                timetonextpoint = timefromlastpoint - self._dwelltime
                if point < len(self._frequencies):
                    frequency = self._frequencies[point]
                else:
                    frequency = self._frequencies[-1]

        return self.response(message, frequency=frequency, time=reftime, timefromlastpoint=timefromlastpoint,
                             timetonextpoint=timetonextpoint)

    # TODO: implement some methods to query modulation state
    def stopModulation(self, message):
        return self.response(message)

    def setFrequencyModulation(self, message):
        if not self.hasRequiredParams(message, ["modulationfrequency", "deviation"]):
            return self.missingPararmResponse(message, ["modulationfrequency", "deviation"])

        if "shape" in message:
            modulationshpae = message["shape"]
        else:
            modulationshpae = ModulationShape.Sine

        return self.response(message)

    def setAmplitudeModulation(self, message):
        if not self.hasRequiredParams(message, ["modulationfrequency", "depth"]):
            return self.missingPararmResponse(message, ["modulationfrequency", "depth"])

        if "shape" in message:
            modulationshape = message["shape"]
        else:
            modulationshape = ModulationShape.Sine

        return self.response(message)

if __name__ == "__main__":
    if (not len(sys.argv) == 2 or
            sys.argv[1] not in ["start", "stop", "restart", "check"]):
        print "Usage: python RfSourceDaemon.py [start/stop/restart/check]"
        sys.exit(0)

    context = zmq.Context()
    cmdsocket = context.socket(zmq.REQ)
    cmdsocket.setsockopt(zmq.LINGER, 0)
    cmdsocket.connect("tcp://localhost:%s" % App.Config()["instrument"]["rfsource"]["cmdport"])

    # check if the daemon is running
    isrunning = False
    ping = {"cmd": "ping"}
    print "Sending command [%s]" % str(ping)
    cmdsocket.send_pyobj(ping)
    time.sleep(1)
    try:
        pong = cmdsocket.recv_pyobj(zmq.DONTWAIT)
        print "Received response [%s]; daemon is running." % str(pong)
        isrunning = True
    except zmq.ZMQError:
        print "Did not receive response to ping [%s]; daemon is not running." % str(ping)

    # if the daemon is running, stop it
    if sys.argv[1] in ["stop", "restart"]:
        if isrunning:
            quitcommand = {"cmd": "quit"}
            print "Sending command [%s]" % str(quitcommand)
            cmdsocket.send_pyobj(quitcommand)
            response = cmdsocket.recv_pyobj()
            print "Received response to command [%s]" % str(response)

            if sys.argv[1] == "restart":
                time.sleep(1)

    # start/restart the daemon if needed
    if sys.argv[1] in ["start", "restart"]:
        try:
            if sys.argv[1] == "restart" or not isrunning:
                if App.Config()["instrument"]["rfsource"]["usedummy"]:
                    daemon = RfSourceDummyDaemon(context)
                else:
                    daemon = RfSourceDaemon(context)
                daemon.run()
                time.sleep(0.1)
                print "Stopped RF source daemon..."
        except Exception as e:
            print "Failed to start RF source daemon due to error [%s]." % str(e)