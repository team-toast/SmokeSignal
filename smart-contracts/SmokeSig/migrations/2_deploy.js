var SmokeSignal = artifacts.require("./SmokeSignal.sol");

module.exports = function(deployer) {
    return deployer.deploy(SmokeSignal, "0x90F8bf6A479f320ead074411a4B0e7944Ea8c9C1");
}