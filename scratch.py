sEMPTY = frozenset()

class Event(object):
    def __init__(self, name):
        self.name = name

TICK = Event("tick")
sTICK = frozenset((TICK,))

TAU = Event("tau")
sTAU = frozenset((TAU,))

class Process(object):
    def __init__(self):
        self.cached_inits = None
        self.cached_after = {}

    def inits(self):
        if self.cached_inits is None:
            self.cached_inits = self._inits()
        return self.cached_inits

    def after(self, event):
        try:
            return self.cached_after[event]
        except KeyError:
            result = self.after(event)
            self.cached_after[event] = result
            return result

    def _inits(self):
        pass

    def _after(self, event):
        pass

class Stop(Process):
    def _inits(self):
        return sEMPTY

    def _after(self, event):
        return sEMPTY

STOP = Stop()
sSTOP = frozenset(STOP)

class Skip(Process):
    def _inits(self):
        return sTICK

    def _after(self, event):
        if event is TICK:
            return sSTOP
        else:
            return sEMPTY

SKIP = Skip()
sSKIP = frozenset(SKIP)

class Compose(Process):
    def __init__(self, left, right):
        Process.__init__(self)
        self.left = left
        self.right = right
        self.inits = None

    def _inits(self):
        if self.inits is None:
            l_inits = self.left.inits()
            if TICK not in l_inits:
                self.inits = l_inits
            else:
                self.inits = (l_inits - sTICK) | sTAU

        return self.inits

    def _after(self, event):
        result = set()

        if event is not TICK:
            result.update(Compose(l, self.right)
                          for l in self.left.after(event))

        if event is TAU and TICK in self.left.inits():
            result.add(self.right)

        return frozenset(result)

class Interrupt(Process):
    def __init__(self, left, right):
        Process.__init__(self)
        self.left = left
        self.right = right

    def _inits(self):
        return self.left.inits() | self.right.inits()

    def _after(self, event):
        result = set()

        result.update(Interrupt(l, self.right)
                      for l in self.left.after(event))

        if event is TAU:
            result.update(Interrupt(self.left, r)
                          for r = self.right.after(event))
        else:
            result.update(self.right.after(event))

        return frozenset(result)

class IntChoice(Process):
    def __init__(self, left, right):
        Process.__init__(self)
        self.left = left
        self.right = right

    def _inits(self):
        return sTAU

    def _after(self, event):
        if event is sTAU:
            return frozenset((self.left, self.right))

        else:
            return sEMPTY

class ExtChoice(Process):
    def __init__(self, left, right):
        Process.__init__(self)
        self.left = left
        self.right = right

    def _inits(self):
        return self.left.inits() | self.right.inits()

    def _after(self, event):
        if event is not TAU:
            return self.left.after(event) | self.right.after(event)

        else:
            result = set()

            result.update(ExtChoice(l, self.right)
                          for l in self.left.after(event))
            result.update(ExtChoice(self.left, r)
                          for r in self.right.after(event))

            return frozenset(result)

class Interleave(Process):
    def __init__(self, left, right):
        Process.__init__(self)
        self.left = left
        self.right = right

    def _inits(self):
        result = set()
        result.update(self.left.inits() - sTICK)
        result.update(self.right.inits() - sTICK)
        result.update(self.left.inits() & self.right.inits() & sTICK)
        return frozenset(result)

    def _after(self, event):
        result = set()

        if event is not TICK:
            result.update(Interleave(l, self.right)
                          for l in self.left.after(event))
            result.update(Interleave(self.left, r)
                          for r in self.right.after(event))

        else:
            result.update(Interleave(l, r)
                          for l in self.left.after(event)
                          for r in self.right.after(event))

class Parallel1(Process):
    def __init__(self, left, alpha, right):
        Process.__init__(self)
        self.left = left
        self.alpha = alpha
        self.right = right

    def _inits(self):
        alphatick = self.alpha | sTICK
        result = set()
        result.update(self.left.inits() - alphatick)
        result.update(self.right.inits() - alphatick)
        result.update(self.left.inits() & self.right.inits() & alphatick)
        return frozenset(result)

    def _after(self, event):
        alphatick = self.alpha | sTICK
        result = set()
        if event not in alphatick:
            result.update(Parallel1(l, self.alpha, self.right)
                          for l in self.left.after(event))
            result.update(Parallel1(self.left, self.alpha, r)
                          for r in self.right.after(event))
        else:
            result.update(Parallel1(l, self.alpha, r)
                          for l in self.left.after(event)
                          for r in self.right.after(event))
        return frozenset(result)


class Parallel2(Process):
    def __init__(self, left, lalpha, ralpha, right):
        Process.__init__(self)
        self.left = left
        self.lalpha = lalpha
        self.ralpha = ralpha
        self.right = right

    def _inits(self):
        result = set()
        result.update(self.left.inits() & (self.lalpha - self.ralpha + sTAU))
        result.update(self.right.inits() & (self.ralpha - self.lalpha + sTAU))
        result.update(self.left.inits() &
                      self.right.inits() &
                      (self.lalpha & self.ralpha & sTICK))
        return frozenset(result)

    def _after(self, event):
        result = set()

        if event in (self.lalpha - self.ralpha + sTAU):
            result.update(Parallel2(l, self.lalpha, self.ralpha, self.right)
                          for l in self.left.after(event))

        if event in (self.ralpha - self.lalpha + sTAU):
            result.update(Parallel2(self.left, self.lalpha, self.ralpha, r)
                          for r in self.right.after(event))

        if event in (self.lalpha & self.ralpha & sTICK):
            result.update(Parallel2(l, self.lalpha, self.ralpha, r)
                          for l in self.left.after(event)
                          for r in self.right.after(event))

        return frozenset(result)

class Hide(Process):
    def __init__(self, process, alpha):
        Process.__init__(self)
        self.process = process
        self.alpha = alpha

    def _inits(self):
        result = self.process.inits()
        if (result & self.alpha) != sEMPTY:
            return result - self.alpha + sTAU
        else:
            return result

    def _after(self, event):
        result = set()

        if event is TAU:
            result.update(Hide(p, self.alpha)
                          for hidden_event in self.alpha
                          for p in self.process.after(hidden_event))

        if event not in self.alpha:
            result.update(self.process.after(event))

        return frozenset(result)


class Rename(Process):
    def __init__(self, process, rel):
        Process.__init__(self)
        self.process = process
        self.rel = rel

    def _inits(self):
        result = self.process.inits() & frozenset((TICK, TAU))
        result.update(self.rel.rimage(self.process.inits()))
        return result

    def _after(self, event):
        if event is TICK or event is TAU:
            return self.process.after(event)

        else:
            result = set()
            result.update(Rename(p, self.rel)
                          for rel_event in self.rel.dimage(event)
                          for p in self.process.after(rel_event))
            return frozenset(result)


class Prefix(Process):

    def __init__(self, events, process_func):
        Process.__init__(self)

        if isinstance(events, Event):
            self.events = frozenset(events)
        else:
            self.events = events

        if isinstance(process_func, Process):
            self.process_func = lambda event: return process_func
        else:
            self.process_func = process_func

    def _inits(self):
        return self.events

    def _after(self, event):
        return self.process_func(event)
