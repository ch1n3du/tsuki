// TODO
// use std::collections::HashMap;

// use crate::{
//     ast::{CallArg, Span},
//     error::Error,
// };

// pub struct FieldMap {
//     pub arity: usize,
//     pub fields: HashMap<String, (usize, Span)>,
//     pub is_function: bool,
// }

// impl FieldMap {
//     pub fn new(arity: usize, is_function: bool) -> Self {
//         Self {
//             arity,
//             is_function,
//             fields: HashMap::new(),
//         }
//     }

//     pub fn insert(&mut self, label: String, index: usize, location: &Span) -> Result<(), Error> {
//         match self.fields.insert(label.clone(), (index, *location)) {
//             Some((_, other_location)) => {
//                 if self.is_function {
//                     Err(Error::DuplicateArgument {
//                         label: label,
//                         location: *location,
//                         duplicate_location: other_location,
//                     })
//                 } else {
//                     Err(Error::DuplicateField {
//                         label: label,
//                         location: *location,
//                         duplicate_location: other_location,
//                     })
//                 }
//             }
//             None => Ok(()),
//         }
//     }

//     pub fn into_option(self) -> Option<Self> {
//         if self.fields.is_empty() {
//             None
//         } else {
//             Some(self)
//         }
//     }

//     pub fn reorder<A>(&self, args: &mut [CallArg<A>], location: Span) -> Result<(), Error> {
//         todo!()
//     }
// }
