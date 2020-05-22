exception Error_Invalid_State

let merge = (a, b) => {
  
  let a_length = Array.length(a);
  let b_length = Array.length(b);
  let res = Array.make(a_length + b_length, None);

  let rec helper = (a_index, b_index) => {
    let a' = a_index < a_length ? Some(Array.get(a,a_index)):None;
    let b' = b_index < b_length ? Some(Array.get(b,b_index)):None;
    switch(a',b') {
      | (None, None) => res
      | (Some(_a_val), None) => {
        Array.set(res,a_index + b_index, a');
        helper(a_index+1,b_index);
      }
      | (Some(a_val), Some(b_val)) when a_val < b_val => {
        Array.set(res, a_index + b_index, a');
        helper(a_index + 1, b_index);
      }
      | (_, Some(_b)) => {
        Array.set(res, a_index + b_index, b');
        helper(a_index, b_index+ 1);
      }
    }
  }

  helper(0,0) |> Array.map(e=> {
    switch e {
      | Some(v) => v
      | None => raise(Error_Invalid_State);
    };
  })
}

let mergeSort = (arr) => {
  let rec helper =
    fun
    | [||] => [||]
    | [|head|] => [|head|]
    | arr => {
      let length = Array.length(arr);
      let mid = length / 2;
      let a = Array.sub(arr,0,mid);
      let b = Array.sub(arr,mid,length-mid);
      let a' = helper(a);
      let b' = helper(b);
      merge(a',b');
    };

    helper(arr);
}


Js.log(mergeSort([|1,5,7,2,4,3,8,0,9,6|]))