var FakeDai = artifacts.require("./FakeDAI.sol");
var SmokeSignal = artifacts.require("./SmokeSignal.sol");

module.exports = function(deployer) {
    deployer.deploy(FakeDai, "1000000000000000000000000").then(function() {
        return deployer.deploy(SmokeSignal, FakeDai.address, "0x90F8bf6A479f320ead074411a4B0e7944Ea8c9C1")
    })
}