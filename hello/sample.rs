// Info { id: InfoId(0), term: @Agent(A), origin: InitialKnowledge, expansion: None, validation: Trust }
// Info { id: InfoId(1), term: @Agent(B), origin: InitialKnowledge, expansion: None, validation: Trust }
// Info { id: InfoId(2), term: #Agent(s), origin: InitialKnowledge, expansion: None, validation: Trust }
// Info { id: InfoId(3), term: sk(@Agent(A), #Agent(s)), origin: InitialKnowledge, expansion: None, validation: Trust }
// Info { id: InfoId(4), term: @Agent(A), origin: Received { msg_nr: M0, index: 0 }, expansion: None, validation: CheckAgainstKnowledge { id: InfoId(0) } }
// Info { id: InfoId(5), term: @Agent(B), origin: Received { msg_nr: M0, index: 1 }, expansion: None, validation: CheckAgainstKnowledge { id: InfoId(1) } }
// Info { id: InfoId(6), term: @Number(NB), origin: Received { msg_nr: M0, index: 2 }, expansion: None, validation: Trust }
// Info { id: InfoId(7), term: @Number(NA), origin: Initiated { msg_nr: M1 }, expansion: DontExpand, validation: Trust }
// Info { id: InfoId(8), term: {| :(@SymmetricKey(KAB), @Agent(B), @Number(NA)) |}sk(@Agent(A), #Agent(s)), origin: Received { msg_nr: M2, index: 0 }, expansion: ExpandedAt { nr: M2, depends_on: [InfoId(3)] }, validation: Trust }
// Info { id: InfoId(9), term: {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh)) |}sk(@Agent(B), #Agent(s)), origin: Received { msg_nr: M2, index: 1 }, expansion: ExpandedAt { nr: M4, depends_on: [InfoId(14)] }, validation: Trust }
// Info { id: InfoId(10), term: :(@SymmetricKey(KAB), @Agent(B), @Number(NA)), origin: DecryptSymmetric { term: InfoId(8), key: InfoId(3), after_msg: M2 }, expansion: ExpandedAt { nr: M2, depends_on: [] }, validation: Trust }
// Info { id: InfoId(11), term: @SymmetricKey(KAB), origin: Decompose { from: InfoId(10), index: 0, after_msg: M2 }, expansion: None, validation: Trust }
// Info { id: InfoId(12), term: @Agent(B), origin: Decompose { from: InfoId(10), index: 1, after_msg: M2 }, expansion: None, validation: CheckAgainstKnowledge { id: InfoId(1) } }
// Info { id: InfoId(13), term: @Number(NA), origin: Decompose { from: InfoId(10), index: 2, after_msg: M2 }, expansion: None, validation: CheckAgainstKnowledge { id: InfoId(7) } }
// Info { id: InfoId(14), term: sk(@Agent(B), #Agent(s)), origin: Received { msg_nr: M4, index: 0 }, expansion: None, validation: Trust }
// Info { id: InfoId(15), term: :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh)), origin: DecryptSymmetric { term: InfoId(9), key: InfoId(14), after_msg: M4 }, expansion: ExpandedAt { nr: M4, depends_on: [] }, validation: Trust }
// Info { id: InfoId(16), term: @SymmetricKey(KAB), origin: Decompose { from: InfoId(15), index: 0, after_msg: M4 }, expansion: None, validation: CheckAgainstKnowledge { id: InfoId(11) } }
// Info { id: InfoId(17), term: @Agent(A), origin: Decompose { from: InfoId(15), index: 1, after_msg: M4 }, expansion: None, validation: CheckAgainstKnowledge { id: InfoId(0) } }
// Info { id: InfoId(18), term: @Number(NB), origin: Decompose { from: InfoId(15), index: 2, after_msg: M4 }, expansion: None, validation: CheckAgainstKnowledge { id: InfoId(6) } }
// Info { id: InfoId(19), term: #Agent(s), origin: Decompose { from: InfoId(15), index: 3, after_msg: M4 }, expansion: None, validation: CheckAgainstKnowledge { id: InfoId(2) } }
// Info { id: InfoId(20), term: @Number(Shhh), origin: Decompose { from: InfoId(15), index: 4, after_msg: M4 }, expansion: None, validation: Trust }

/// The knowledge required for agent A
struct Knowledge<T: Terms> {
    /// @Agent(A)
    i0: Option<Agent<T::Agent>>,
    /// @Agent(B)
    i1: Option<Agent<T::Agent>>,
    /// #Agent(s)
    i2: Option<Agent<T::Agent>>,
    /// sk(@Agent(A), #Agent(s))
    i3: Option<Func<T::User_sk, (Agent<T::Agent>, Agent<T::Agent>)>>,
    /// @Agent(A)
    i4: Option<Agent<T::Agent>>,
    /// @Agent(B)
    i5: Option<Agent<T::Agent>>,
    /// @Number(NB)
    i6: Option<Number<T::Number>>,
    /// @Number(NA)
    i7: Option<Number<T::Number>>,
    // / {| :(@SymmetricKey(KAB), @Agent(B), @Number(NA)) |}sk(@Agent(A), #Agent(s))
    i8: Option<
        SymEnc<
            Tuple<(
                SymmetricKey<T::SymmetricKey>,
                Agent<T::Agent>,
                Number<T::Number>,
            )>,
            Func<T::User_sk, (Agent<T::Agent>, Agent<T::Agent>)>,
        >,
    >,
    /// {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh)) |}sk(@Agent(B), #Agent(s))
    i9: Option<
        SymEnc<
            Tuple<(
                SymmetricKey<T::SymmetricKey>,
                Agent<T::Agent>,
                Number<T::Number>,
                Agent<T::Agent>,
                Number<T::Number>,
            )>,
            Func<T::User_sk, (Agent<T::Agent>, Agent<T::Agent>)>,
        >,
    >,
    /// :(@SymmetricKey(KAB), @Agent(B), @Number(NA))
    i10: Option<
        Tuple<(
            SymmetricKey<T::SymmetricKey>,
            Agent<T::Agent>,
            Number<T::Number>,
        )>,
    >,
    /// @SymmetricKey(KAB)
    i11: Option<SymmetricKey<T::SymmetricKey>>,
    /// @Agent(B)
    i12: Option<Agent<T::Agent>>,
    /// @Number(NA)
    i13: Option<Number<T::Number>>,
    /// sk(@Agent(B), #Agent(s))
    i14: Option<Func<T::User_sk, (Agent<T::Agent>, Agent<T::Agent>)>>,
    /// :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh))
    i15: Option<
        Tuple<(
            SymmetricKey<T::SymmetricKey>,
            Agent<T::Agent>,
            Number<T::Number>,
            Agent<T::Agent>,
            Number<T::Number>,
        )>,
    >,
    /// @SymmetricKey(KAB)
    i16: Option<SymmetricKey<T::SymmetricKey>>,
    /// @Agent(A)
    i17: Option<Agent<T::Agent>>,
    /// @Number(NB)
    i18: Option<Number<T::Number>>,
    /// #Agent(s)
    i19: Option<Agent<T::Agent>>,
    /// @Number(Shhh)
    i20: Option<Number<T::Number>>,
}

// Retrieve { index: 0, id: InfoId(4) }
// Compare { trusted: InfoId(0), new: InfoId(4) }
// Retrieve { index: 1, id: InfoId(5) }
// Compare { trusted: InfoId(1), new: InfoId(5) }
// Retrieve { index: 2, id: InfoId(6) }
// Initiate(InfoId(7))
// Send { to: s, terms: [InfoId(0), InfoId(1), InfoId(7), InfoId(6)] }
// Retrieve { index: 0, id: InfoId(8) }
// Retrieve { index: 1, id: InfoId(9) }
// DecryptSymmetric { term: InfoId(8), key: InfoId(3), into: InfoId(10) }
// Extract { from: InfoId(10), index: 0, into: InfoId(11) }
// Extract { from: InfoId(10), index: 1, into: InfoId(12) }
// Compare { trusted: InfoId(1), new: InfoId(12) }
// Extract { from: InfoId(10), index: 2, into: InfoId(13) }
// Compare { trusted: InfoId(7), new: InfoId(13) }
// Send { to: B, terms: [InfoId(9)] }
// Retrieve { index: 0, id: InfoId(14) }
// DecryptSymmetric { term: InfoId(9), key: InfoId(14), into: InfoId(15) }
// Extract { from: InfoId(15), index: 0, into: InfoId(16) }
// Compare { trusted: InfoId(11), new: InfoId(16) }
// Extract { from: InfoId(15), index: 1, into: InfoId(17) }
// Compare { trusted: InfoId(0), new: InfoId(17) }
// Extract { from: InfoId(15), index: 2, into: InfoId(18) }
// Compare { trusted: InfoId(6), new: InfoId(18) }
// Extract { from: InfoId(15), index: 3, into: InfoId(19) }
// Compare { trusted: InfoId(2), new: InfoId(19) }
// Extract { from: InfoId(15), index: 4, into: InfoId(20) }

mod agent_a {
    /// Ingoing messages
    enum Ingoing<T: Terms> {
        /// Message { terms: [@Agent(A), @Agent(B), @Number(NB)] }
        M0(Agent<T::Agent>, Agent<T::Agent>, Number<T::Number>),
        /// Message { terms: [{| :(@SymmetricKey(KAB), @Agent(B), @Number(NA)) |}sk(@Agent(A), #Agent(s)), {| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh)) |}sk(@Agent(B), #Agent(s))] }
        M2(
            SymEnc<
                Tuple<(
                    SymmetricKey<T::SymmetricKey>,
                    Agent<T::Agent>,
                    Number<T::Number>,
                )>,
                Func<T::User_sk, (Agent<T::Agent>, Agent<T::Agent>)>,
            >,
            SymEnc<
                Tuple<(
                    SymmetricKey<T::SymmetricKey>,
                    Agent<T::Agent>,
                    Number<T::Number>,
                    Agent<T::Agent>,
                    Number<T::Number>,
                )>,
                Func<T::User_sk, (Agent<T::Agent>, Agent<T::Agent>)>,
            >,
        ),
        /// Message { terms: [sk(@Agent(B), #Agent(s))] }
        M4(Func<T::User_sk, (Agent<T::Agent>, Agent<T::Agent>)>),
    }
    /// Outgoing messages
    enum Outgoing<'a, T: Terms> {
        /// Message { terms: [@Agent(A), @Agent(B), @Number(NA), @Number(NB)] }
        M1(
            &'a Agent<T::Agent>,
            &'a Agent<T::Agent>,
            &'a Number<T::Number>,
            &'a Number<T::Number>,
        ),
        /// Message { terms: [{| :(@SymmetricKey(KAB), @Agent(A), @Number(NB), #Agent(s), @Number(Shhh)) |}sk(@Agent(B), #Agent(s))] }
        M3(
            &'a SymEnc<
                Tuple<(
                    SymmetricKey<T::SymmetricKey>,
                    Agent<T::Agent>,
                    Number<T::Number>,
                    Agent<T::Agent>,
                    Number<T::Number>,
                )>,
                Func<T::User_sk, (Agent<T::Agent>, Agent<T::Agent>)>,
            >,
        ),
    }

    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    struct Agent<A>(A);
    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    struct Func<F, A>(F, A);
    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    struct Number<N>(N);
    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    struct Tuple<Inner>(Inner);
    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    struct SymEnc<Body, Key>(Body, Key);
    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    struct AsymEnc<Body, Key>(Body, Key);
    #[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
    struct SymmetricKey<S>(S);

    fn work<T: Terms>(base: &mut T, msg: &T::Payload) -> Result<()> {
        let mut knowledge = Knowledge {
            i0: None,
            i1: None,
            i2: None,
            i3: None,
            i4: None,
            i5: None,
            i6: None,
            i7: None,
            i8: None,
            i9: None,
            i10: None,
            i11: None,
            i12: None,
            i13: None,
            i14: None,
        };
        let msg = base.deserialize(msg)?;
        progress(base, &mut knowledge, msg);

        Ok(())
    }
    fn wow() -> Result<()> {
        let mut base = SuperBase {};
        work(&mut base, &"hello world".to_string())?;
        Ok(())
    }

    fn progress<'a, T: Terms>(
        base: &mut T,
        knowledge: &'a mut Knowledge<T>,
        msg: Ingoing<T>,
    ) -> Result<Outgoing<'a, T>> {
        match msg {
            Ingoing::M0(m0, m1, m2) => {
                // Retrieve { index: 0, id: InfoId(4) }
                knowledge.i4 = Some(m0);
                // Compare { trusted: InfoId(0), new: InfoId(4) }
                assert_eq!(knowledge.i0, knowledge.i4);
                // Retrieve { index: 1, id: InfoId(5) }
                knowledge.i5 = Some(m1);
                // Compare { trusted: InfoId(1), new: InfoId(5) }
                assert_eq!(knowledge.i1, knowledge.i5);
                // Retrieve { index: 2, id: InfoId(6) }
                knowledge.i6 = Some(m2);

                // Sending message to s
                // Initiate(InfoId(7))
                knowledge.i7 = Some(base.init());
                // Send { to: s, terms: [InfoId(0), InfoId(1), InfoId(7), InfoId(6)] }
                return Ok(Outgoing::M1(
                    knowledge.i0.as_ref().unwrap(),
                    knowledge.i1.as_ref().unwrap(),
                    knowledge.i7.as_ref().unwrap(),
                    knowledge.i6.as_ref().unwrap(),
                ));
            }
            Ingoing::M2(m0, m1) => {
                // Retrieve { index: 0, id: InfoId(8) }
                knowledge.i8 = Some(m0);
                // Retrieve { index: 1, id: InfoId(9) }
                knowledge.i9 = Some(m1);
                // DecryptSymmetric { term: InfoId(8), key: InfoId(3), into: InfoId(10) }
                knowledge.i10 = Some(&base.symmetric_decrypt(
                    knowledge.i8.as_ref().unwrap(),
                    knowledge.i3.as_ref().unwrap(),
                )?);
                // Extract { from: InfoId(10), index: 0, into: InfoId(11) }
                knowledge.i11 = Some(knowledge.i10.as_ref().unwrap().0 .0.clone());
                // Extract { from: InfoId(10), index: 1, into: InfoId(12) }
                knowledge.i12 = Some(knowledge.i10.as_ref().unwrap().0 .1.clone());
                // Compare { trusted: InfoId(1), new: InfoId(12) }
                assert_eq!(knowledge.i1, knowledge.i12);
                // Extract { from: InfoId(10), index: 2, into: InfoId(13) }
                knowledge.i13 = Some(knowledge.i10.as_ref().unwrap().0 .2.clone());
                // Compare { trusted: InfoId(7), new: InfoId(13) }
                assert_eq!(knowledge.i7, knowledge.i13);

                // Sending message to B
                // Send { to: B, terms: [InfoId(9)] }
                return Ok(Outgoing::M3(knowledge.i9.as_ref().unwrap()));
            }
            Ingoing::M4(m0) => {
                // Retrieve { index: 0, id: InfoId(14) }
                knowledge.i14 = Some(m0);
                // DecryptSymmetric { term: InfoId(9), key: InfoId(14), into: InfoId(15) }
                knowledge.i15 = Some(base.symmetric_decrypt(
                    knowledge.i9.as_ref().unwrap(),
                    knowledge.i14.as_ref().unwrap(),
                )?);
                // Extract { from: InfoId(15), index: 0, into: InfoId(16) }
                knowledge.i16 = Some(knowledge.i15.as_ref().unwrap().0 .0.clone());
                // Compare { trusted: InfoId(11), new: InfoId(16) }
                assert_eq!(knowledge.i11, knowledge.i16);
                // Extract { from: InfoId(15), index: 1, into: InfoId(17) }
                knowledge.i17 = Some(knowledge.i15.as_ref().unwrap().0 .1.clone());
                // Compare { trusted: InfoId(0), new: InfoId(17) }
                assert_eq!(knowledge.i0, knowledge.i17);
                // Extract { from: InfoId(15), index: 2, into: InfoId(18) }
                knowledge.i18 = Some(knowledge.i15.as_ref().unwrap().0 .2.clone());
                // Compare { trusted: InfoId(6), new: InfoId(18) }
                assert_eq!(knowledge.i6, knowledge.i18);
                // Extract { from: InfoId(15), index: 3, into: InfoId(19) }
                knowledge.i19 = Some(knowledge.i15.as_ref().unwrap().0 .3.clone());
                // Compare { trusted: InfoId(2), new: InfoId(19) }
                assert_eq!(knowledge.i2, knowledge.i19);
                // Extract { from: InfoId(15), index: 4, into: InfoId(20) }
                knowledge.i20 = Some(knowledge.i15.as_ref().unwrap().0 .4.clone());
                todo!()
            }
        }
    }
}
