import { parse } from "papaparse";

export const parserConfig = {
  dynamicTyping: true,
  header: true,
  skipEmptyLines: true,
};

export const parseFile = (file) => {
  return new Promise((resolve, reject) => {
    parse(file, {
      ...parserConfig,
      complete: (parseResults) => {
        if (parseResults === null) {
          reject();
        }
        resolve(parseResults);
      },
    });
  });
};
