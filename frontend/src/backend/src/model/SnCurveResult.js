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
 * The SnCurveResult model module.
 * @module model/SnCurveResult
 * @version 0.1.0
 */
class SnCurveResult {
  /**
   * Constructs a new <code>SnCurveResult</code>.
   * @alias module:model/SnCurveResult
   * @param outputs {Object.<String, File>}
   */
  constructor(outputs) {
    SnCurveResult.initialize(this, outputs);
  }

  /**
   * Initializes the fields of this object.
   * This method is used by the constructors of any subclasses, in order to implement multiple inheritance (mix-ins).
   * Only for internal use.
   */
  static initialize(obj, outputs) {
    obj["outputs"] = outputs;
  }

  /**
   * Constructs a <code>SnCurveResult</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/SnCurveResult} obj Optional instance to populate.
   * @return {module:model/SnCurveResult} The populated <code>SnCurveResult</code> instance.
   */
  static constructFromObject(data, obj) {
    if (data) {
      obj = obj || new SnCurveResult();

      if (data.hasOwnProperty("outputs")) {
        obj["outputs"] = ApiClient.convertToType(data["outputs"], {
          String: File,
        });
      }
      if (data.hasOwnProperty("plot")) {
        obj["plot"] = ApiClient.convertToType(data["plot"], Object);
      }
    }
    return obj;
  }
}

/**
 * @member {Object.<String, File>} outputs
 */
SnCurveResult.prototype["outputs"] = undefined;

/**
 * @member {Object} plot
 */
SnCurveResult.prototype["plot"] = undefined;

export default SnCurveResult;