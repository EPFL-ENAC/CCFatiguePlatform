export function getOutputFileName(input, output, inputName, method) {
  const name = inputName
    .replace(new RegExp(`^(${input})`), "")
    .replace(/\.[^/.]+$/, "");
  return `${output}${name}_${method}`;
}
