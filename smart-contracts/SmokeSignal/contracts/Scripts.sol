pragma solidity ^0.6.0;
pragma experimental ABIEncoderV2;

import "./SmokeSignal.sol";

contract Scripts {
    function getBulkAccounting(SmokeSignal smokeSignal, bytes32[] calldata messageIds)
        external
        view
        returns
            (address[] memory firstAuthorArray,
             uint[] memory nativeBurnedArray,
             uint[] memory dollarsBurnedArray,
             uint[] memory nativeTippedArray,
             uint[] memory dollarsTippedArray
            )
        {
            for (uint i=0; i<messageIds.length; i++) {
                (firstAuthorArray[i],
                 nativeBurnedArray[i],
                 dollarsBurnedArray[i],
                 nativeTippedArray[i],
                 dollarsTippedArray[i]
                ) = smokeSignal.storedMessageData(messageIds[i]);
            }
        }
}