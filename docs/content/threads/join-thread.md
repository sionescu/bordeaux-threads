---
date: 2022-01-07T08:00:00Z
title: 'Function JOIN-THREAD'
weight: 7
---

#### Syntax:

**join-thread** thread => multiple values

#### Arguments and values:

*thread* -> a [thread](../class-thread) object.

#### Description

Wait until `thread` terminates, or if it has already terminated,
return immediately.

The return values of the thread function are returned.

#### Examples


```
(let ((thread (bt2:make-thread
               (lambda () (values 1 2 3)))))
  (bt2:join-thread thread))

```
=> 1, 2, 3

#### Exceptional situations:

If a thread is terminated by a condition, then the condition
[**abnormal-exit**](../abnormal-exit) is signaled.

#### See also:

[**make-thread**](./make-thread),
[**abnormal-exit**](../abnormal-exit)

#### Notes:

None.
