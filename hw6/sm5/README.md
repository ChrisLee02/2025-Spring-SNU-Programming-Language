# SM5

## 컴파일 및 실행 방법

`machine.ml`에는 SM5 기계가 정의되어 있고, `k.ml`에는 K-\* 인터프리터가 구현되어 있습니다. 아래와 같이 실행하면, 주어진 K-\* 프로그램을 여러분이 작성하신 번역기에 따라 번역하고 SM5 기계로 실행합니다.

```sh
λ dune build --release
λ ./_build/default/bin/main.exe examples/test1.k-star
```

혹은 정의된 `run` 링크를 사용하여

```sh
λ ./run examples/test1.k-star
```

## K-\* 문법 나무 출력하기

주어진 K-\* 프로그램의 문법 나무를 화면에 출력해주는 모듈이 `pp.ml` 파일에 포함되어 있습니다. 이를 통해 파싱이 의도한대로 되고 있는지 확인해보실 수 있습니다.

```sh
λ ./run -pk examples/test1.k-star
```

## K-\* 실행기로 실행하기

주어진 K-\* 프로그램을 K-\* 실행기로 실행한 결과를 다음과 같이 확인할 수 있습니다.
번역이 제대로 되었다면, SM5 기계로 실행한 결과와 K-\* 실행기로 실행한 결과가 같아야 합니다.

```sh
λ ./run -k examples/test1.k-star
```

## 번역된 SM5 프로그램 출력하기

주어진 K-\* 프로그램을 SM5로 번역한 결과를 `-psm5` 옵션을 통해 출력할 수 있습니다.

```sh
λ ./run -psm5 examples/test1.k-star
```

## SM5 기계 위에서 디버그 모드로 실행하기

주어진 K-\* 프로그램을 SM5로 번역한 다음, 디버그 모드에서 실행합니다. 디버그 모드는 SM5의 매 단계마다 기계 상태를 출력합니다. 출력되는 문자열의 양이 많으므로 주의하세요.

```sh
λ ./run -debug examples/test1.k-star
```

## GC가 달린 SM5로 실행하기

주어진 K-\* 프로그램을 SM5로 번역한 다음, GC가 장착된 SM5 기계 위에서 실행합니다.

```sh
λ ./run -gc examples/test1.k-star
```

## 숙제 제출 방법

1. SM5 문제는 `translator.ml` 파일 안 `trans` 함수를 완성한 후, `translator.ml` 파일만 제출해주세요.
2. SM5 Limited 문제는 `machine.ml` 안 `malloc_with_gc` 함수를 완성한 후, `machine.ml` 파일만 제출해주세요.
