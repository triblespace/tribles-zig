// test {
//   NS([_]Attr{
//     .{.id = "", .type = u64},
//     .{}
//   })
//   Query(.{})
//   find(({ name, title }) => [
//       ,
//     ]).run()

// bookNS.find(.{v(.name), v(.title)})
// 	     .in(characterkb.where(.{
// 		  .{ .name = v(.name),
// 		     .loves = .{.{v(.juliet)}}},
// 	  	  .{ ._id = v(.juliet)
// 		     .name = "juliet",
// 		     .titles = .{v(.title)}}}))

//   kb.pull(id, T)
//   kb.walk(id).get(.name)
// }

// fn myQuery(variable) anytype {
//  return .{

//  }
// }

// struct {
//   field: Value(Int)
//   field2: Entity(User)
// }

// Partial(T){}