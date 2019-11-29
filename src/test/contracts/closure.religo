/* Test whether closures retain values in Reasonligo */

let test = (k: int): int => {
  let j: int = k + 5;
  let close: (int => int) = (i: int) => i + j;

  let j: int = 20; /* Shadow original variable to see if value close'd */
  close(20);
};
