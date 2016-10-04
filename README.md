[![Travis CI Status](https://travis-ci.org/skedgeme/next-ref.svg?branch=master)](http://travis-ci.org/skedgeme/next-ref)

A concurrency primitive for a slow consumer that can tolerate missing some updates.

`next-ref` provides blocking and non-blocking reads of a value.

Useful when the consumer is expensive to run, and you want to limit it's use to
only necessary updates, and exceptional circumstances require reading without
blocking.
