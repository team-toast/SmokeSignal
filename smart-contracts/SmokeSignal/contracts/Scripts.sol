pragma solidity ^0.6.0;
pragma experimental ABIEncoderV2;

import "./SmokeSignal.sol";

contract Scripts {
    function getBulkStoredMessageData(SmokeSignal smokeSignal, bytes32[] calldata messageIds)
        external
        view
        returns (StoredMessageData[] memory messageData)
        {
            messageData = new StoredMessageData[](messageIds.length);
            for (uint i=0; i<messageIds.length; i++) {
                (messageData[i].firstAuthor,
                 messageData[i].nativeBurned,
                 messageData[i].dollarsBurned,
                 messageData[i].nativeTipped,
                 messageData[i].dollarsTipped
                ) = smokeSignal.storedMessageData(messageIds[i]);
            }
        }
}