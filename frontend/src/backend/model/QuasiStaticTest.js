import ApiClient from "../ApiClient";

/**
 * The QuasiStaticTest model module.
 * @module model/QuasiStaticTest
 * @version 0.1.0
 */
class QuasiStaticTest {
  /**
   * Constructs a new <code>QuasiStaticTest</code>.
   */
  constructor(
    specimenName,
    crackDisplacement,
    crackLoad,
    crackLength,
    displacement,
    load,
    strain,
    stress,
    specimenId,
    toughness,
    initial_crack_length,
  ) {
    QuasiStaticTest.initialize(
      this,
      specimenName,
      crackDisplacement,
      crackLoad,
      crackLength,
      displacement,
      load,
      strain,
      stress,
      specimenId,
      toughness,
      initial_crack_length
    );
  }

  static initialize(
    obj,
    specimenName,
    crackDisplacement,
    crackLoad,
    crackLength,
    displacement,
    load,
    strain,
    stress,
    specimenId,
    toughness,
    initial_crack_length
  ) {
    obj["specimen_name"] = specimenName;
    obj["crack_displacement"] = crackDisplacement;
    obj["crack_load"] = crackLoad;
    obj["crack_length"] = crackLength;
    obj["displacement"] = displacement;
    obj["load"] = load;
    obj["strain"] = strain;
    obj["stress"] = stress;
    obj["specimen_id"] = specimenId;
    obj["toughness"] = toughness;
    obj["initial_crack_length"] = initial_crack_length;
  }

  static constructFromObject(data, obj) {
    if (data) {
      console.log("Raw QuasiStaticTest data:", data); // useful logs
      obj = obj || new QuasiStaticTest();

      if (data.hasOwnProperty("specimen_name")) {
        obj["specimen_name"] = ApiClient.convertToType(data["specimen_name"], "String");
      }
      if (data.hasOwnProperty("crack_displacement")) {
        obj["crack_displacement"] = ApiClient.convertToType(data["crack_displacement"], ["Number"]);
      }
      if (data.hasOwnProperty("crack_load")) {
        obj["crack_load"] = ApiClient.convertToType(data["crack_load"], ["Number"]);
      }
      if (data.hasOwnProperty("crack_length")) {
        obj["crack_length"] = ApiClient.convertToType(data["crack_length"], ["Number"]);
      }
      if (data.hasOwnProperty("displacement")) {
        obj["displacement"] = ApiClient.convertToType(data["displacement"], { String: ["Number"] });
      }
      if (data.hasOwnProperty("load")) {
        obj["load"] = ApiClient.convertToType(data["load"], { String: ["Number"] });
      }
      if (data.hasOwnProperty("strain")) {
        obj["strain"] = ApiClient.convertToType(data["strain"], { String: ["Number"] });
      }
      if (data.hasOwnProperty("stress")) {
        obj["stress"] = ApiClient.convertToType(data["stress"], { String: ["Number"] });
      }
      if (data.hasOwnProperty("specimen_id")) {
        obj["specimen_id"] = ApiClient.convertToType(data["specimen_id"], "Number");
      }
      if (data.hasOwnProperty("toughness")) {
        obj["toughness"] = ApiClient.convertToType(data["toughness"], "Number");
      }
      if (data.hasOwnProperty("initial_crack_length")) {
        obj["initial_crack_length"] = ApiClient.convertToType(data["initial_crack_length"], "Number");
      }
    }
    console.log("Constructed QuasiStaticTest object:", obj); // useful logs
    return obj;
  }
}

/** Properties */
QuasiStaticTest.prototype["specimen_name"] = undefined;
QuasiStaticTest.prototype["crack_displacement"] = undefined;
QuasiStaticTest.prototype["crack_load"] = undefined;
QuasiStaticTest.prototype["crack_length"] = undefined;
QuasiStaticTest.prototype["displacement"] = undefined;
QuasiStaticTest.prototype["load"] = undefined;
QuasiStaticTest.prototype["strain"] = undefined;
QuasiStaticTest.prototype["stress"] = undefined;
QuasiStaticTest.prototype["specimen_id"] = undefined;
QuasiStaticTest.prototype["toughness"] = undefined;
QuasiStaticTest.prototype["initial_crack_length"] = undefined;

export default QuasiStaticTest;
