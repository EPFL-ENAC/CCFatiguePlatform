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
 * The AppInfo model module.
 * @module model/AppInfo
 * @version 0.1.0
 */
class AppInfo {
  /**
   * Constructs a new <code>AppInfo</code>.
   * App Info
   * @alias module:model/AppInfo
   * @param name {String}
   * @param version {String}
   */
  constructor(name, version) {
    AppInfo.initialize(this, name, version);
  }

  /**
   * Initializes the fields of this object.
   * This method is used by the constructors of any subclasses, in order to implement multiple inheritance (mix-ins).
   * Only for internal use.
   */
  static initialize(obj, name, version) {
    obj["name"] = name;
    obj["version"] = version;
  }

  /**
   * Constructs a <code>AppInfo</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/AppInfo} obj Optional instance to populate.
   * @return {module:model/AppInfo} The populated <code>AppInfo</code> instance.
   */
  static constructFromObject(data, obj) {
    if (data) {
      obj = obj || new AppInfo();

      if (data.hasOwnProperty("name")) {
        obj["name"] = ApiClient.convertToType(data["name"], "String");
      }
      if (data.hasOwnProperty("version")) {
        obj["version"] = ApiClient.convertToType(data["version"], "String");
      }
    }
    return obj;
  }
}

/**
 * @member {String} name
 */
AppInfo.prototype["name"] = undefined;

/**
 * @member {String} version
 */
AppInfo.prototype["version"] = undefined;

export default AppInfo;