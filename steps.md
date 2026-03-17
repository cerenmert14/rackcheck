

- git clone rackcheck
- `cd rackcheck`
- run `raco pkg remove rackcheck`
- run `raco pkg remove rackcheck-lib`
- `cd` into clone
    - `cd rackcheck-lib`
        - run `raco pkg install`
    - `cd ../rackcheck`
        - run `raco pkg install`
- modify `rackcheck-lib/rackunit.rkt`
- work with `examples/coin.rkt` until you can generate the following json
    - run `raco test examples/coin.rkt`

```json
{
    "discards": 943,
    "foundbug": true,
    "passed": 732,
    "time": 0.055253000000000003
}
```