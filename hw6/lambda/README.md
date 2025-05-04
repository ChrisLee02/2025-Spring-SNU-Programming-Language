# Lambda Ground

## 컴파일 및 실행 방법

```sh
λ dune build --release
λ ./_build/default/bin/main.exe examples/test1.l
```

혹은 정의된 `run` 링크를 사용하여

```sh
λ ./run examples/test1.l
```

을 실행하면 입력파일을 파싱한 결과와 normal-order reduction을 적용한 결과를 같이 보여줍니다.
테스트용 프로그램을 만드실 때는 괄호를 잘 이용하시기 바랍니다.

실행시 파일명을 명시하지 않을 경우, 표준 입력으로부터 실행할 코드를 읽어들입니다.
표준 입력으로 프로그램을 입력하신 후, 첫 번째 칸(column)에서 Ctrl-D를 누르면 프로그램이 실행됩니다.

## 숙제 제출 방법

`evaluator.ml` 파일에 있는 `reduce` 함수를 완성하시고 그 파일만 제출해 주세요.
