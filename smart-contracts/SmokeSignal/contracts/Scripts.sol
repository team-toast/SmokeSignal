pragma solidity ^0.6.0;
pragma experimental ABIEncoderV2;

import "./SmokeSignal.sol";

contract Scripts {
    function getBulkAccounting(
        SmokeSignal smokeSignal,
        bytes32[] calldata messageIds
    )
        external
        view
        returns (
            address[] memory firstAuthorArray,
            uint256[] memory nativeBurnedArray,
            uint256[] memory dollarsBurnedArray,
            uint256[] memory nativeTippedArray,
            uint256[] memory dollarsTippedArray
        )
    {
        uint256 size = messageIds.length;

        firstAuthorArray = new address[](size);
        nativeBurnedArray = new uint256[](size);
        dollarsBurnedArray = new uint256[](size);
        nativeTippedArray = new uint256[](size);
        dollarsTippedArray = new uint256[](size);

        for (uint256 i = 0; i < size; i++) {
            (
                firstAuthorArray[i],
                nativeBurnedArray[i],
                dollarsBurnedArray[i],
                nativeTippedArray[i],
                dollarsTippedArray[i]
            ) = smokeSignal.storedMessageData(messageIds[i]);
        }
    }
}