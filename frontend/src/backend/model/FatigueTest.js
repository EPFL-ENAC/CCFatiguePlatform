/**
 * ccfatigue
 * No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)
 *
 * The version of the OpenAPI document: 0.1.0
 *
 *
 * NOTE: This class is auto generated by OpenAPI Generator (https://openapi-generator.tech).
 * https://openapi-generator.tech
 * Do not edit the class manually.
 *
 */

import ApiClient from "../ApiClient";
import HysteresisLoop from "./HysteresisLoop";

/**
 * The FatigueTest model module.
 * @module model/FatigueTest
 * @version 0.1.0
 */
class FatigueTest {
  /**
   * Constructs a new <code>FatigueTest</code>.
   * @alias module:model/FatigueTest
   * @param specimenId {Number}
   * @param totalDissipatedEnergy {Number}
   * @param hysteresisLoops {Array.<module:model/HysteresisLoop>}
   * @param nCycles {Array.<Number>}
   * @param creep {Array.<Number>}
   * @param hysteresisArea {Array.<Number>}
   * @param stiffness {Array.<Number>}
   */
  constructor(
    specimenId,
    totalDissipatedEnergy,
    hysteresisLoops,
    nCycles,
    creep,
    hysteresisArea,
    stiffness
  ) {
    FatigueTest.initialize(
      this,
      specimenId,
      totalDissipatedEnergy,
      hysteresisLoops,
      nCycles,
      creep,
      hysteresisArea,
      stiffness
    );
  }

  /**
   * Initializes the fields of this object.
   * This method is used by the constructors of any subclasses, in order to implement multiple inheritance (mix-ins).
   * Only for internal use.
   */
  static initialize(
    obj,
    specimenId,
    totalDissipatedEnergy,
    hysteresisLoops,
    nCycles,
    creep,
    hysteresisArea,
    stiffness
  ) {
    obj["specimen_id"] = specimenId;
    obj["total_dissipated_energy"] = totalDissipatedEnergy;
    obj["hysteresis_loops"] = hysteresisLoops;
    obj["n_cycles"] = nCycles;
    obj["creep"] = creep;
    obj["hysteresis_area"] = hysteresisArea;
    obj["stiffness"] = stiffness;
  }

  /**
   * Constructs a <code>FatigueTest</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/FatigueTest} obj Optional instance to populate.
   * @return {module:model/FatigueTest} The populated <code>FatigueTest</code> instance.
   */
  static constructFromObject(data, obj) {
    if (data) {
      obj = obj || new FatigueTest();

      if (data.hasOwnProperty("specimen_id")) {
        obj["specimen_id"] = ApiClient.convertToType(
          data["specimen_id"],
          "Number"
        );
      }
      if (data.hasOwnProperty("total_dissipated_energy")) {
        obj["total_dissipated_energy"] = ApiClient.convertToType(
          data["total_dissipated_energy"],
          "Number"
        );
      }
      if (data.hasOwnProperty("hysteresis_loops")) {
        obj["hysteresis_loops"] = ApiClient.convertToType(
          data["hysteresis_loops"],
          [HysteresisLoop]
        );
      }
      if (data.hasOwnProperty("n_cycles")) {
        obj["n_cycles"] = ApiClient.convertToType(data["n_cycles"], ["Number"]);
      }
      if (data.hasOwnProperty("creep")) {
        obj["creep"] = ApiClient.convertToType(data["creep"], ["Number"]);
      }
      if (data.hasOwnProperty("hysteresis_area")) {
        obj["hysteresis_area"] = ApiClient.convertToType(
          data["hysteresis_area"],
          ["Number"]
        );
      }
      if (data.hasOwnProperty("stiffness")) {
        obj["stiffness"] = ApiClient.convertToType(data["stiffness"], [
          "Number",
        ]);
      }
    }
    return obj;
  }
}

/**
 * @member {Number} specimen_id
 */
FatigueTest.prototype["specimen_id"] = undefined;

/**
 * @member {Number} total_dissipated_energy
 */
FatigueTest.prototype["total_dissipated_energy"] = undefined;

/**
 * @member {Array.<module:model/HysteresisLoop>} hysteresis_loops
 */
FatigueTest.prototype["hysteresis_loops"] = undefined;

/**
 * @member {Array.<Number>} n_cycles
 */
FatigueTest.prototype["n_cycles"] = undefined;

/**
 * @member {Array.<Number>} creep
 */
FatigueTest.prototype["creep"] = undefined;

/**
 * @member {Array.<Number>} hysteresis_area
 */
FatigueTest.prototype["hysteresis_area"] = undefined;

/**
 * @member {Array.<Number>} stiffness
 */
FatigueTest.prototype["stiffness"] = undefined;

export default FatigueTest;