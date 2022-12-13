struct Agent_A<A, B, S, SkAS, KAB> {
    // Initial knowledge
    _A: A,
    _B: B,
    _s: s,
    _sk: SkAS,
    // Knowledge after Msg1
    // Knowledge after Msg2
    _KAB: Option<KAB>, // Knowledge after Msg3
}
struct Agent_B<A, B, S, SkBS, KAB> {
    // Initial knowledge
    _A: A,
    _B: B,
    _s: s,
    _sk: SkBS,
    // Knowledge after Msg1
    // Knowledge after Msg2
    // Knowledge after Msg3
    _KAB: Option<KAB>,
}
struct Agent_s<A, B, S, SkAS, SkBS, KAB> {
    // Initial knowledge
    _A: A,
    _B: B,
    _s: s,
    _skA: SkAS,
    _skB: SkBS,
    // Knowledge after Msg1
    _KAB: Option<KAB>,
    // Knowledge after Msg2
    // Knowledge after Msg3
}

struct Msg1<A, B> {
    _A: A,
    _B: B,
}
struct Msg2<KAB> {
    _KAB: KAB,
}
struct Msg3<A, KAB> {
    _A: A,
    _KAB: KAB,
}

impl<A, B, S, SkAS, KAB> Agent_A<A, B, S, SkAS, KAB> {
    fn step_1<E>(&mut self, exe: &mut E) -> Msg1<A, B>
    where
        E: Executor<A = A, B = B, KAB = KAB>,
    {
        Msg1 {
            _A: exe.give_me_an_a(),
            _B: exe.give_me_a_b(),
        }
    }
}

trait Crypto {
    fn symmetric_encryption(&mut self, input: &[u8]) -> Vec<u8>;
    fn asymmetric_encryption(&mut self, input: &[u8]) -> Vec<u8>;
}

trait Executor {
    type Crypto: Crypto;

    type A: Serialize + Deserialize;
    type B: Serialize + Deserialize;
    type KAB: Serialize + Deserialize;

    fn init(base: Crypto) -> Self;

    fn give_me_an_a(&mut self) -> A;
    fn give_me_a_b(&mut self) -> B;
}

// END AUTO GENERATED

struct PlainText {}

impl Crypto for PlainText {
    fn symmetric_encryption(&mut self, input: &[u8]) -> Vec<u8> {
        input.to_vec()
    }
}
struct NialpText {}
impl Crypto for NialpText {
    fn symmetric_encryption(&mut self, input: &[u8]) -> Vec<u8> {
        let mut output = input.to_vec();
        output.reverse();
        output
    }
}

struct HttpExecutor {}

impl Executor for HttpExecutor {
    type Crypto = NialpText;
    type TransportLayer = HttpLayer;

    type A = IpAddress;
    type B = IpAddress;
    type KAB = Vec<u8>;
}
