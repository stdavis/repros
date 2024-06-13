import angularPreset from "conventional-changelog-angular";
import { Bumper } from "conventional-recommended-bump";

const preset = await angularPreset();
const bumper = new Bumper(process.cwd()).loadPreset({
  ...preset,
  name: "angular",
});

const recommendation = await bumper.bump();

console.log("script completed successfully");
