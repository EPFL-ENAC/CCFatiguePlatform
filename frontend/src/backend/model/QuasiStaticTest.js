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
    experimentMetadata // ✅ added
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
      experimentMetadata // ✅ added
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
    experimentMetadata // ✅ added
  ) {
    obj["specimen_name"] = specimenName;
    obj["crack_displacement"] = crackDisplacement;
    obj["crack_load"] = crackLoad;
    obj["crack_length"] = crackLength;
    obj["displacement"] = displacement;
    obj["load"] = load;
    obj["strain"] = strain;
    obj["stress"] = stress;
    obj["experiment_metadata"] = experimentMetadata; // ✅ added
  }

  static constructFromObject(data, obj) {
    if (data) {
      obj = obj || new QuasiStaticTest();

      if (data.hasOwnProperty("specimen_name")) {
        obj["specimen_name"] = ApiClient.convertToType(
          data["specimen_name"],
          "String"
        );
      }
      if (data.hasOwnProperty("crack_displacement")) {
        obj["crack_displacement"] = ApiClient.convertToType(
          data["crack_displacement"],
          ["Number"]
        );
      }
      if (data.hasOwnProperty("crack_load")) {
        obj["crack_load"] = ApiClient.convertToType(data["crack_load"], [
          "Number",
        ]);
      }
      if (data.hasOwnProperty("crack_length")) {
        obj["crack_length"] = ApiClient.convertToType(data["crack_length"], [
          "Number",
        ]);
      }
      if (data.hasOwnProperty("displacement")) {
        obj["displacement"] = ApiClient.convertToType(data["displacement"], {
          String: ["Number"],
        });
      }
      if (data.hasOwnProperty("load")) {
        obj["load"] = ApiClient.convertToType(data["load"], {
          String: ["Number"],
        });
      }
      if (data.hasOwnProperty("strain")) {
        obj["strain"] = ApiClient.convertToType(data["strain"], {
          String: ["Number"],
        });
      }
      if (data.hasOwnProperty("stress")) {
        obj["stress"] = ApiClient.convertToType(data["stress"], {
          String: ["Number"],
        });
      }
      if (data.hasOwnProperty("experiment_metadata")) { // ✅ added
        obj["experiment_metadata"] = ApiClient.convertToType(
          data["experiment_metadata"],
          { String: "Any" }
        );
      }
    }
    return obj;
  }
}

/**
 * @member {String} specimen_name
 */
QuasiStaticTest.prototype["specimen_name"] = undefined;
/**
 * @member {Array.<Number>} crack_displacement
 */
QuasiStaticTest.prototype["crack_displacement"] = undefined;
/**
 * @member {Array.<Number>} crack_load
 */
QuasiStaticTest.prototype["crack_load"] = undefined;
/**
 * @member {Array.<Number>} crack_length
 */
QuasiStaticTest.prototype["crack_length"] = undefined;
/**
 * @member {Object.<String, Array.<Number>>} displacement
 */
QuasiStaticTest.prototype["displacement"] = undefined;
/**
 * @member {Object.<String, Array.<Number>>} load
 */
QuasiStaticTest.prototype["load"] = undefined;
/**
 * @member {Object.<String, Array.<Number>>} strain
 */
QuasiStaticTest.prototype["strain"] = undefined;
/**
 * @member {Object.<String, Array.<Number>>} stress
 */
QuasiStaticTest.prototype["stress"] = undefined;
/**
 * @member {Object.<String, Any>} experiment_metadata
 */
QuasiStaticTest.prototype["experiment_metadata"] = undefined; // ✅ added

export default QuasiStaticTest;
