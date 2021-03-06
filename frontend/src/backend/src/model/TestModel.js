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

/**
 * The TestModel model module.
 * @module model/TestModel
 * @version 0.1.0
 */
class TestModel {
  /**
   * Constructs a new <code>TestModel</code>.
   * Defines how test is seen on the API
   * @alias module:model/TestModel
   * @param id {Number}
   * @param experimentId {Number}
   */
  constructor(id, experimentId) {
    TestModel.initialize(this, id, experimentId);
  }

  /**
   * Initializes the fields of this object.
   * This method is used by the constructors of any subclasses, in order to implement multiple inheritance (mix-ins).
   * Only for internal use.
   */
  static initialize(obj, id, experimentId) {
    obj["id"] = id;
    obj["experiment_id"] = experimentId;
  }

  /**
   * Constructs a <code>TestModel</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/TestModel} obj Optional instance to populate.
   * @return {module:model/TestModel} The populated <code>TestModel</code> instance.
   */
  static constructFromObject(data, obj) {
    if (data) {
      obj = obj || new TestModel();

      if (data.hasOwnProperty("id")) {
        obj["id"] = ApiClient.convertToType(data["id"], "Number");
      }
      if (data.hasOwnProperty("experiment_id")) {
        obj["experiment_id"] = ApiClient.convertToType(
          data["experiment_id"],
          "Number"
        );
      }
      if (data.hasOwnProperty("specimen_number")) {
        obj["specimen_number"] = ApiClient.convertToType(
          data["specimen_number"],
          "String"
        );
      }
      if (data.hasOwnProperty("stress_ratio")) {
        obj["stress_ratio"] = ApiClient.convertToType(
          data["stress_ratio"],
          "Number"
        );
      }
      if (data.hasOwnProperty("maximum_stress")) {
        obj["maximum_stress"] = ApiClient.convertToType(
          data["maximum_stress"],
          "Number"
        );
      }
      if (data.hasOwnProperty("loading_rate")) {
        obj["loading_rate"] = ApiClient.convertToType(
          data["loading_rate"],
          "Number"
        );
      }
      if (data.hasOwnProperty("run_out")) {
        obj["run_out"] = ApiClient.convertToType(data["run_out"], "Boolean");
      }
    }
    return obj;
  }
}

/**
 * @member {Number} id
 */
TestModel.prototype["id"] = undefined;

/**
 * @member {Number} experiment_id
 */
TestModel.prototype["experiment_id"] = undefined;

/**
 * @member {String} specimen_number
 */
TestModel.prototype["specimen_number"] = undefined;

/**
 * @member {Number} stress_ratio
 */
TestModel.prototype["stress_ratio"] = undefined;

/**
 * @member {Number} maximum_stress
 */
TestModel.prototype["maximum_stress"] = undefined;

/**
 * @member {Number} loading_rate
 */
TestModel.prototype["loading_rate"] = undefined;

/**
 * @member {Boolean} run_out
 */
TestModel.prototype["run_out"] = undefined;

export default TestModel;
