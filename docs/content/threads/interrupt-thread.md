---
date: 2022-01-07T08:00:00Z
title: 'Function INTERRUPT-THREAD'
weight: 11
---

#### Syntax:

**interrupt-thread** thread function *&rest* arguments => thread

#### Arguments and values:

*thread* -> a [thread](../class-thread) object.\
*function* -> a function object.\
*arguments* -> a list of values.

#### Description:

Interrupt `thread` and execute `function` within its dynamic context
by applying `function` to `arguments` before continuing with the
interrupted path of execution.

Returns the thread object it acted on.

#### Exceptional situations:

None.

#### See also:

[**make-thread**](../make-thread), [**join-thread**](../join-thread)

#### Notes:

This may not be a good idea if `thread` is holding locks or doing
anything important.
