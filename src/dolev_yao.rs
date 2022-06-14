use crate::{
    ast, dolev_yao,
    protocol::{Func, Message, Stage, TypedStage, UntypedStage},
};

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub struct Knowledge<S: Stage = TypedStage>(Vec<Message<S>>);

impl<S: Stage> Knowledge<S> {
    pub fn new(mut msgs: Vec<Message<S>>) -> Self {
        msgs.sort_unstable();
        msgs.dedup();

        let mut k = Knowledge(msgs);

        augment_knowledge(&mut k, &[]);

        k
    }
    pub fn iter(&self) -> impl Iterator<Item = &Message<S>> {
        self.0.iter()
    }

    pub fn augment_with(&mut self, msg: Message<S>) {
        self.0.push(msg);
        self.0.sort_unstable();
        self.0.dedup();

        augment_knowledge(self, &[]);
    }
}
impl<S: Stage> FromIterator<Knowledge<S>> for Knowledge<S> {
    fn from_iter<T: IntoIterator<Item = Knowledge<S>>>(iter: T) -> Self {
        let mut xs: Vec<_> = iter.into_iter().flat_map(|x| x.0).collect();
        Knowledge::new(xs)
    }
}
impl<S: Stage> FromIterator<Message<S>> for Knowledge<S> {
    fn from_iter<T: IntoIterator<Item = Message<S>>>(iter: T) -> Self {
        Knowledge::new(iter.into_iter().collect())
    }
}

impl<'a> From<Vec<ast::Message<'a>>> for Knowledge<UntypedStage> {
    fn from(knowledge: Vec<ast::Message<'a>>) -> Self {
        Self(knowledge.into_iter().map(Message::from).collect())
    }
}

pub fn composition_search<S: Stage>(
    knowledge: &Knowledge<S>,
    goal: &Message<S>,
    public_functions: &[Func],
) -> bool {
    match goal {
        Message::Composition { func, args } => {
            public_functions.contains(func)
                && args
                    .iter()
                    .all(|arg| composition_search(knowledge, arg, public_functions))
        }
        _ => {
            if knowledge.0.contains(goal) {
                true
            } else {
                dbg!(goal);
                false
            }
        }
    }
}

pub fn augment_knowledge<S: Stage>(knowledge: &mut Knowledge<S>, public_functions: &[Func]) {
    loop {
        let mut new_messages = Vec::new();

        for message in knowledge.iter() {
            match message {
                Message::SymEnc { message, key } => {
                    if composition_search(knowledge, key, public_functions) {
                        new_messages.push(*message.clone());
                    }
                }
                Message::AsymEnc { message, key } => {
                    if let Message::Inverse(_key) = key.as_ref() {
                        new_messages.push(*message.clone());
                    } else if composition_search(
                        knowledge,
                        &Message::Inverse((*key).clone()),
                        public_functions,
                    ) {
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
            return;
        }

        knowledge.0.append(&mut new_messages);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        parse::{parse_message, parse_messages},
        protocol::UntypedStage,
    };

    #[test]
    fn simple_composition_slides_example() {
        /*
           init knowledge: {k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }
           goal: h(k_1, k_2)


           axiom      axiom
           --------  ---------
           M |- k_1  M |- k_2
           -------------------
           M |- h(k_1, k_2)
        */

        let goal: Message<UntypedStage> = parse_message("h(k1, k2)").unwrap().into();

        let knowledge: Knowledge<UntypedStage> =
            parse_messages("k1, k2, {|n1, k3|}h(k1,k2), {|n2|}k3")
                .unwrap()
                .into(); // {k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }

        assert!(composition_search(
            &knowledge,
            &goal,
            &[Func("h".to_string())]
        ));
    }

    #[test]
    fn non_provable_composition() {
        /*
           init knowledge: {k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }
           goal: h(k_1, k_3)
        */
        let goal: Message<UntypedStage> = parse_message("h(k1, k3)").unwrap().into();

        let knowledge: Knowledge<UntypedStage> =
            parse_messages("k1, k2, {|n1, k3|}h(k1, k2), {|n2|}k3")
                .unwrap()
                .into();

        assert!(!composition_search(
            &knowledge,
            &goal,
            &[Func("h".to_string())]
        ));
    }

    #[test]
    fn non_public_function() {
        /*
           init knowledge: {k1, k_2 }
           pub: h
           goal: f(k_1, k_2)
        */
        let goal: Message<UntypedStage> = parse_message("f(k1, k2)").unwrap().into();

        let knowledge: Knowledge<UntypedStage> = parse_messages("k1, k2").unwrap().into();

        assert!(!composition_search(
            &knowledge,
            &goal,
            &[Func("h".to_string())]
        ));
    }

    #[test]
    fn augment_knowledge_slides_example() {
        /*
           init knowledge: { k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3 }
           final knowledge: { k1, k2, {|<n1, k3>|}h(k1,k2), {|n2|}k3, <n1, k3>, n1, k3, n2 }
        */
        let mut knowledge: Knowledge<UntypedStage> =
            parse_messages("k1, k2, {|n1, k3|}h(k1,k2), {|n2|}k3")
                .unwrap()
                .into();
        augment_knowledge(&mut knowledge, &[Func("h".to_string())]);

        assert_eq!(
            knowledge,
            parse_messages("k1, k2, {|n1, k3|}h(k1,k2), {|n2|}k3, n1, k3, n1, k3, n2")
                .unwrap()
                .into()
        );

        let mut knowledge: Knowledge<UntypedStage> =
            parse_messages("k1, k2, {|n1, k3|}h(k1,k2), {|n2|}k3 ")
                .unwrap()
                .into();
        augment_knowledge(&mut knowledge, &[Func("h".to_string())]);

        assert_eq!(
            knowledge,
            parse_messages("k1, k2, {|n1, k3|}h(k1, k2), {|n2|}k3, n1, k3, n1, k3, n2")
                .unwrap()
                .into()
        );
    }
}
