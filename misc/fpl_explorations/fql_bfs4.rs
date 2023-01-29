
use Queue;
// Need:
fn Queue::new<T>() -> Queue<T>;
fn Queue::is_empty<T>(&Queue<T>) -> bool;
fn Queue::insert<T>(&mut Queue<T>, T) -> bool;
fn Queue::pop<T>(&mut Queue<T>) -> T;

use List;
// Need:
fn List::singleton<T>(T) -> List<T>;
fn List::last<T>(&List<T>) -> &T; // could raise if empty
fn List::append<T>(&mut List<T>, T);
fn List::remove_last<T>(&mut List<T>) -> T;


trait Graph {
  fn expand(&N) -> Iterator<N>;
}

fn bfs<N: Graph>(is_destination: fn(&N) -> bool, start_node: N) -> Option<List<N>> {
  let mut queue = Queue::new();
  queue.insert(List::singleton(start_node));
  while ! with const queue { Queue::is_empty(&queue) } {
    let curr_path: List<N> = Queue::pop(&mut queue);
    let curr_node: &N = List::last(&curr_path);
    if is_destination(curr_node) {
      return Some(curr_path);
    }
    else {
      for new_node in Graph::expand(curr_node) {
        let mut new_path = curr_path;
        List::append(&mut new_path, new_node);
        queue.insert(new_path);
      }
    }
  }
  return None;
}
