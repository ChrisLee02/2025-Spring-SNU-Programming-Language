1. 람다 계산법 - 기계적 계산에 대한 알론소 처치의 대답
- 괴델의 재귀, 튜링머신, 람다 계산법은 모두 equivalent
- 람다식의 재귀적 정의, 자유변수와 치환, Context: 구멍이 뚫린 람다식
- 람다의 세계에서 프로그램의 실행은 식의 변형이다. 그 규칙을 정의해보자: 람다식의 전이 semantics
- Beta reduction: 인자에 값을 치환해서 계산하기
- ???: sub-expression을 beta reduction할 수 있다면 그 결과로 Context도 reduction.
- Alpha conversion: 변수명 치환하기
- 함수의 apply에 있어서 결합법칙이 성립하지 않음. 순서에 따라 다른 결과가 나올 수 있다.
- normal form -> (람다로 감싸지 않은 람다식, value) 은 유일하게 존재한다? Confluence theorem?
- 람다 계산에서는 알파 변환이 결과에 영향을 주지 않아야 함. dynamic-scoping은 meaningless
- (\x.fxx) (\x.fxx) -> 재귀함수를 인코딩하는 방법. Y combinator라고 부릅니다.
- Yf = f(Yf), fixed point combinator? 씨!!!!!!!!!!!!!!!발!!!!!!!!!!!!!!!
- Y라는게, 임의의 함수 f를 받아서 그 f에 대한 (\x.f(xx))(\x.f(xx))를 만듬. G = (\x.f(xx))로 정의하면 GG = f(GG)가 됨. 이게 자기복제를 통해 재귀함수로 작용함. 베이스 케이스스 
- normal form이 있는 경우에 그것에 도달할 수 있는 계산의 순서가 존재한다.
- normal-form reduction -> 가장 바깥의, 가장 왼쪽부터 계산을 적용한다. 
- 람다로 부품 만들기 - 정수, 불리언, 논리연산, 사칙연산,,
- 튜링은 학부생 때 튜링머신을 고안했고, 추천서를 받아 알론소 처치의 아래에서 박사과정을 하게 됨. 이때 처치-튜링 동치를 증명하고 처치-튜링의 방식이 전부라는 가설을 제시함.(처치 - 튜링 thesis)
- 모든 기계적 계산은 순수한 람다 함수로 표현 가능. 함수의 정의와 적용(Beta-reduction)이 계산의 전부다.
- reduction <-> evaluation, normal-form <-> canonical form, lambda-term <-> closed lambda-term
- normal-order vs eager-order evaluation 
- normal-order evaluation rules -> canonical form은 그대로, 베타리덕션이 가능한건 실행. (lazy eval, call-by-name)
- eager-order evaluation -> 베타리덕션의 대상 인자가 canonical form일때만 리덕션을 실행한다. (call-by-value)
- lazy와 eager중 뭐가 더 낫냐,, 답이 읎다.
- eager는 normal sequence가 아님.. 그래서 Y comb가 안끝날것,,

2. 람다의 세계에서 의미를 정의해보자.
- Env, Val을 비슷한 형태로 정의. 이것은 sound & complete, 완전하고 안전한 구현입니다.
- 일단 eager-eval로 언어를 정의. 함수의 정의와 적용, 재귀(z라는 놈을 끼워넣어야 한다. 슬라이드 참고), branch, 정수 연산 등을 정의.
- 이런저런 설탕들을 붙이기. pair, malloc, IO, 반복, 등등...
- malloc을 위한 s를 정의 (side-effect)
- Val = N + B + Val * Val + Loc + Exp * Env
- 여기서도 마찬가지, 의미 정의에서는 잡아내지 못하는 타입 제약이 있음.
- 타입 시스템,, 이번엔 잘 좀 만들어보자,,ㅋ 
Q. 왜 람다의 세계에서는 타입이 잘 정의될 수 있을까? 튜링기계의 세계에서는 왜 잘 안되는걸까?
-> 람다의 세계에서는 계산이 인풋과 아웃풋 그 자체로 정의되니까? 흠,,

- simple type system -> let-polymorphism type system으로 발전시켜나가자.

3. 타입 시스템
- 단순한 expression 정의(값, 함수, 유닛, 어플라이, let 바인딩), 타입 정의 (단일 타입, 타입->타입 함수.)
- 각 정의들에 대응하는 타입 추론의 규칙을 정한다. 
- 단순 타입 시스템의 문제점 - 다형성을 표현할 수 없다? f라는 함수가 돌멩이와 사람을 받을 수 있는 경우는 세상에 많다. 저울, 시소, 등등,,, 마찬가지로 프로그래밍에서 .age를 호출하도록 하는 코드..
- 짜잔 ㅡ 보물섬 문제는 사실 단순 타입 시스템이었습니다~
- 이 시스템은 type safety를 보장하는가? where type safety = e가 문제없이 돌고, 만약에 끝난다면 최종 타입의 값을 낸다.

4. 단순 타입 시스템
- (alpha, beta) = alpha를 풀려면, alpha = (a -> a -> a-> ... -> a -> b ) a가 무한하다면,,
- 타입 추론의 안전성을 증명하기,, Progress + Subject Reduction(= Preservation)
-> Progress는 최종 값이 나올 때까지 e가 e -> e' 로 진행된다는 것, Preservation은 e->e'으로 변한 후에도 e'의 타입이 보존된다는 것
그 두 개를 맞물리면 "e가 잘 돌고 끝난다면, 그 타입이 타우다."라는 Soundness(=안전성)을 증명 가능..
- C = Evaluation Context(실행 문맥)
C ::= []                    // 구멍 자체
    | C e                   // 문맥이 왼쪽에 있고, 오른쪽에 식 e를 붙임
    | v C                   // 값 v가 왼쪽에 있고, 문맥이 오른쪽에 붙음
    | v + C
    | C + e
    | λx. C                 // 람다식 안에 문맥이 있음 ->   
e → e'
-----------
C[e] → C[e']
- Progress lemma의 증명

5. let-polymorphic type system
- 타입을 하나의 값만 갖는게 아닌 다형성을 부여,, 이러면 좀 더 유연하게 OK사인을 주는게 가능해짐.
- let을 이용해서 타입을 명시해준다. 이게 imperative에서와 달리 람다에서는 syntatic sugar임..
- let x = e1 in e2 형태에서, e1에다가 최대한 for all를 붙여준 뒤 e2에서 x에 대한 타입추론의 힌트로 사용한다..
- for all alpha, alpha -> alpha 인 함수의 집합? -> Intersect (tau -> tau) where tau = simple type.
- polymorphism의 rank? rank가 올라가면 완전한 구현은 불가
-> rank 0 = simple type, rank 1 = for all가 가장 바깥에만..