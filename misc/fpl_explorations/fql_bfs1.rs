
use Queue;
// Need:
fn Queue::new<T>() -> Queue<T>;
fn Queue::is_empty<T>(Queue<T>) -> bool;
fn Queue::insert<T>(&mut Queue<T>, T) -> bool;
fn Queue::pop<T>(&mut Queue<T>) -> T; // how can we get a T out of this? isn't it necessarily mutable?

use List;
// Need:
fn List::singleton<T>(T) -> List<T>;
fn List::last<T>(List<T>) -> T; // could raise if empty
fn List::append<T>(&mut List<T>, T);
fn List::remove_last<T>(&mut List<T>) -> T; // can we get a T here?


trait Graph {
  fn expand(N) -> Iterator<N>;
}

fn bfs<N: Graph>(is_destination: fn(N) -> bool, start_node: N) -> Option<List<N>> {
  let mut queue = Queue::new();
  queue.insert(List::singleton(start_node));
  while !*Queue::is_empty(?mut queue) {
    let curr_path = Queue::pop(&mut queue); // wrong!
    let curr_node = List::last(curr_path);
    if is_destination(curr_node) {
      return Some(curr_path);
    }
    else {
      let mut new_path = mutCopy curr_path;
      for new_node in Graph::expand(curr_node) {
        List::append(&mut new_path, new_node);
        queue.insert(constCopy new_path);
        List::remove_last(new_path);
      }
    }
  }
  return None;
}
