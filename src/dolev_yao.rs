use crate::{
    ast,
    protocol::{Constant, Message, Stage, TypedStage, UntypedStage},
};

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub struct Knowledge<S: Stage = TypedStage>(pub Vec<Message<S>>);

impl Knowledge<TypedStage> {
    pub fn new(mut msgs: Vec<Message<TypedStage>>) -> Self {
        msgs.sort_unstable();
        msgs.dedup();

        let mut k = Knowledge(msgs);

        augment_knowledge(&mut k);

        k
    }
    pub fn iter(&self) -> impl Iterator<Item = &Message<TypedStage>> {
        self.0.iter()
    }

    pub fn augment_with(&mut self, msg: Message<TypedStage>) {
        self.0.push(msg);
        self.0.sort_unstable();
        self.0.dedup();

        augment_knowledge(self);
    }
}

impl FromIterator<Knowledge<TypedStage>> for Knowledge<TypedStage> {
    fn from_iter<T: IntoIterator<Item = Knowledge<TypedStage>>>(iter: T) -> Self {
        Knowledge::new(iter.into_iter().flat_map(|x| x.0).collect())
    }
}
impl FromIterator<Message<TypedStage>> for Knowledge<TypedStage> {
    fn from_iter<T: IntoIterator<Item = Message<TypedStage>>>(iter: T) -> Self {
        Knowledge::new(iter.into_iter().collect())
    }
}

impl<'a> From<&[ast::Message<&'a str>]> for Knowledge<UntypedStage<'a>> {
    fn from(knowledge: &[ast::Message<&'a str>]) -> Self {
        Self(knowledge.iter().cloned().map(Message::from).collect())
    }
}
impl<'a> From<Vec<ast::Message<&'a str>>> for Knowledge<UntypedStage<'a>> {
    fn from(knowledge: Vec<ast::Message<&'a str>>) -> Self {
        Self(knowledge.into_iter().map(Message::from).collect())
    }
}

pub fn composition_search(knowledge: &Knowledge<TypedStage>, goal: &Message<TypedStage>) -> bool {
    let mut stack = vec![goal];
    while let Some(message) = stack.pop() {
        if knowledge.0.contains(message) {
            continue;
        }
        match message {
            Message::Composition { func, args } => {
                if !func.is_public()
                    && !knowledge
                        .0
                        .contains(&Message::Constant(Constant::Function(func.clone())))
                {
                    return false;
                }

                for a in args {
                    stack.push(a);
                }
            }
            Message::Tuple(ts) => {
                for a in ts {
                    stack.push(a);
                }
            }
            Message::SymEnc { message, key } => {
                stack.push(message);
                stack.push(key);
            }
            Message::AsymEnc { message, key } => {
                stack.push(message);
                stack.push(key);
            }
            _ => return false,
        }
    }
    true
}

pub fn augment_knowledge(knowledge: &mut Knowledge<TypedStage>) {
    loop {
        let mut new_messages = Vec::new();

        for message in knowledge.iter() {
            match message {
                Message::SymEnc { message, key } => {
                    if composition_search(knowledge, key) {
                        new_messages.push(*message.clone());
                    }
                }
                Message::AsymEnc { message, key } => {
                    if let Message::Inverse(_key) = key.as_ref() {
                        new_messages.push(*message.clone());
                    } else if composition_search(knowledge, &Message::Inverse((*key).clone())) {
                        new_messages.push(*message.clone());
                    }
                }
                Message::Tuple(messages) => {
                    for m in messages {
                        new_messages.push(m.clone());
                    }
                }
                _ => {}
            }
        }

        new_messages.retain(|message| !knowledge.0.contains(message));

        if new_messages.is_empty() {
            knowledge.0.retain(|m| !matches!(m, Message::Tuple(_)));
            knowledge.0.sort_unstable();
            knowledge.0.dedup();
            return;
        }

        knowledge.0.append(&mut new_messages);
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::{
//         parse::{parse_message, parse_messages},
//         protocol::{TypingContext, UntypedStage},
//     };
//
//     #[test]
//     fn simple_composition_slides_example() {
//         /*
//            init knowledge: {k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }
//            goal: h(k_1, k_2)
//
//
//            axiom      axiom
//            --------  ---------
//            M |- k_1  M |- k_2
//            -------------------
//            M |- h(k_1, k_2)
//         */
//
//         let goal: Message<UntypedStage> = parse_message("h(k1, k2)").unwrap().into();
//
//         let knowledge: Knowledge<UntypedStage> =
//             parse_messages("k1, k2, {|n1, k3|}h(k1,k2), {|n2|}k3")
//                 .unwrap()
//                 .into(); // {k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }
//
//         assert!(composition_search(&knowledge, &goal, &[Func("h".into())]));
//     }
//
//     #[test]
//     fn non_provable_composition() {
//         /*
//            init knowledge: {k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }
//            goal: h(k_1, k_3)
//         */
//         let goal: Message<UntypedStage> = parse_message("h(k1, k3)").unwrap().into();
//
//         let knowledge: Knowledge<UntypedStage> =
//             parse_messages("k1, k2, {|n1, k3|}h(k1, k2), {|n2|}k3")
//                 .unwrap()
//                 .into();
//
//         assert!(!composition_search(&knowledge, &goal, &[Func("h".into())]));
//     }
//
//     #[test]
//     fn non_public_function() {
//         /*
//            init knowledge: {k1, k_2 }
//            pub: h
//            goal: f(k_1, k_2)
//         */
//         let goal: Message<UntypedStage> = parse_message("f(k1, k2)").unwrap().into();
//
//         let knowledge: Knowledge<UntypedStage> = parse_messages("k1, k2").unwrap().into();
//
//         assert!(!composition_search(&knowledge, &goal, &[Func("h".into())]));
//     }
//
//     #[test]
//     fn augment_knowledge_slides_example() {
//         /*
//            init knowledge: { k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }
//            final knowledge: { k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3, <n1, k3>, n1, k3, n2 }
//         */
//         let mut knowledge: Knowledge<UntypedStage> =
//             parse_messages("k1, k2, {|n1, k3|}h(k1,k2), {|n2|}k3")
//                 .unwrap()
//                 .into();
//         augment_knowledge(&mut knowledge, &[Func("h".into())]);
//
//         assert_eq!(
//             knowledge,
//             parse_messages("k1, k2, {|n1, k3|}h(k1,k2), {|n2|}k3, n1, k3, n1, k3, n2")
//                 .unwrap()
//                 .into()
//         );
//
//         let mut knowledge: Knowledge<UntypedStage> =
//             parse_messages("k1, k2, {|n1, k3|}h(k1,k2), {|n2|}k3 ")
//                 .unwrap()
//                 .into();
//         augment_knowledge(&mut knowledge, &[Func("h".into())]);
//
//         assert_eq!(
//             knowledge,
//             parse_messages("k1, k2, {|n1, k3|}h(k1, k2), {|n2|}k3, n1, k3, n1, k3, n2")
//                 .unwrap()
//                 .into()
//         );
//     }
// }
