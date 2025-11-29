# SmokeSignal: Chain Selection & Optimization Research

## Executive Summary

This document summarizes research into optimizing SmokeSignal for gas efficiency, cross-chain bridging, and permanent data availability.

**Key Findings:**
- Gas can be reduced by ~85% using IPFS-based storage
- Intent-based bridging with 5% keeper fee requires zero infrastructure
- Most L2s (Optimism, Base, Arbitrum) lose calldata after 18 days due to EIP-4844 blobs
- **Gnosis Chain remains the best option** for permanent, cheap, uncensorable messaging

---

## Current Contract Analysis

### On-Chain Data Structure

```solidity
struct StoredMessageData {
    address firstAuthor;    // Who posted first (for tipping)
    uint nativeBurned;      // Total ETH/xDAI burned
    uint dollarsBurned;     // USD value burned
    uint nativeTipped;      // Total tips to author
    uint dollarsTipped;     // USD value tipped
}
```

### Message Format (in events)

Messages are JSON with `!smokesignal` prefix:
```json
!smokesignal{"m":["Title","Desc","Body"],"v":3,"c":{"topic":"general"}}
```

Topics and replies are **conventions in the JSON**, not on-chain indexes.

### Current Gas Cost: ~100,000-150,000 per post

---

## Gas Optimization Options

| Approach | Gas | Savings | Trade-off |
|----------|-----|---------|-----------|
| Current | ~100-150k | baseline | Full features |
| Remove storage, event only | ~25k | -83% | No on-chain tipping |
| IPFS hash only | ~22k | -85% | Content on IPFS (needs pinning) |
| Pure calldata (no event) | ~24k | -84% | Harder to index |

### Recommended: IPFS Hash Storage

```solidity
contract SmokeSignalIPFS {
    event Post(bytes32 indexed cid, address indexed author, uint256 burned);

    function post(bytes32 _cid) external payable {
        emit Post(_cid, msg.sender, msg.value);
    }
}
```

**Cost: ~22,000 gas (~$0.002 on Gnosis)**

---

## Cross-Chain Bridging Design

### Intent-Based Pattern (Recommended)

Zero infrastructure required. Uses economic incentives:

```
1. Contract accumulates funds from posts
2. Anyone creates intent: "Have X tokens, want 95% on destination"
3. Fillers compete to fulfill (deliver to recipient)
4. Filler proves delivery, claims source tokens + 5% fee
```

### Proof Mechanism: Optimistic with Bond

```solidity
contract IntentBridge {
    uint256 constant FILLER_BOND = 0.1 ether;
    uint256 constant DISPUTE_PERIOD = 2 hours;
    uint256 constant FILLER_FEE_BPS = 500;  // 5%

    function claimFill(bytes32 intentId, bytes32 destTxHash) external payable {
        require(msg.value >= FILLER_BOND, "Need bond");
        // Filler posts bond, provides destination tx hash as proof
        // Anyone can verify via block explorer during dispute period
    }

    function settle(bytes32 intentId) external {
        // After dispute period, filler claims funds + bond
    }

    function dispute(bytes32 intentId, bytes calldata evidence) external {
        // If filler lied, bond is slashed
    }
}
```

### Price Oracle for Multi-Chain

Use Chainlink price feeds (free to read, gas only):

```solidity
function createIntent(address recipient, uint32 destChain) external {
    uint256 usdValue = (address(this).balance * getChainlinkPrice()) / 1e18;
    uint256 minUsdOut = usdValue * 9500 / 10000;  // 95%
    emit IntentCreated(intentId, usdValue, minUsdOut, recipient, destChain);
}
```

---

## Chain Selection for Permanent Data

### CRITICAL: L2 Blob Expiry Problem

**EIP-4844 introduced "blobs" that expire after ~18 days.**

L2s using blobs will **lose your calldata**:
- Optimism
- Base
- Arbitrum One
- zkSync Era
- Polygon zkEVM
- Scroll
- Linea

**Contract storage persists**, but calldata (where messages live) does not.

### Safe Chains (Data Preserved Forever)

| Chain | Gas Cost | Free Archive? | Chainlink | Verdict |
|-------|----------|---------------|-----------|---------|
| **Gnosis** | $0.0001 | Yes (Gateway) | Yes | **BEST** |
| **Fuse** | $0.0001 | Yes | Limited | Great |
| **Avalanche** | $0.03 | Yes (public) | Yes | Good |
| **Kava EVM** | $0.01 | Yes | Limited | Good |
| **PulseChain** | $0.001 | Yes | No (Fetch Oracle) | Okay |
| **Polygon PoS** | $0.01 | No ($49/mo) | Yes | If paying |
| **Fantom** | $0.001 | No ($49/mo) | Yes | If paying |

### Unsafe Chains (Data Lost)

| Chain | Problem |
|-------|---------|
| Optimism | Blob data expires 18 days |
| Base | Blob data expires 18 days |
| Arbitrum One | Blob data expires 18 days |
| zkSync Era | Blob data expires 18 days |
| Polygon zkEVM | Blob data expires 18 days |
| Arbitrum Nova | DAC trust assumptions (6 members) |
| Harmony | Unstable, 22TB archive, incidents |

---

## Recommended Architecture

### Primary: Gnosis Chain

```
Gnosis Chain
├── Cheapest: $0.0001 per tx
├── Free archive access via Gateway
├── Native stablecoin (xDAI = $1, no oracle needed!)
├── Chainlink price feeds available
├── Same client software as Ethereum
├── Already runs SmokeSignal
└── Data preserved forever
```

### Contract Design

```solidity
// Minimal storage, maximum efficiency
contract SmokeSignalV2 {
    event Post(
        bytes32 indexed contentHash,  // IPFS CID or keccak256
        address indexed author,
        uint256 burned,
        bytes32 indexed parentHash    // For reply threading
    );

    function post(bytes32 _contentHash, bytes32 _parentHash) external payable {
        emit Post(_contentHash, msg.sender, msg.value, _parentHash);
    }
}

// Separate treasury for bridging
contract SmokeSignalTreasury {
    uint256 constant KEEPER_FEE_BPS = 500;  // 5%

    function createIntent(address recipient, uint32 destChain) external {
        // Emit intent for keepers to fill
    }
}
```

### Data Flow

```
User creates post
       │
       ├── Content → IPFS (pinned)
       │
       └── CID + burn amount → Gnosis Chain
                │
                ├── Event emitted (permanent)
                └── ETH accumulated in treasury
                        │
                        └── Keeper bridges to mainnet (5% fee)
```

---

## Chainlink Price Feed Addresses

### Gnosis Chain
```solidity
address constant ETH_USD = 0xa767f745331D267c7751297D982b050c93985627;
address constant BTC_USD = 0x6C1d7e76EF7304a40e8456ce883BC56d3dEA3F7d;
// Note: xDAI = $1 always (stablecoin), no feed needed
```

### Avalanche C-Chain
```solidity
address constant AVAX_USD = 0x0A77230d17318075983913bC2145DB16C7366156;
address constant ETH_USD  = 0x976B3D034E162d8bD72D6b9C989d545b839003b0;
```

### Polygon PoS
```solidity
address constant MATIC_USD = 0xAB594600376Ec9fD91F8e885dADF0CE036862dE0;
address constant ETH_USD   = 0xF9680D99D6C9589e2a93a78A04A279e509205945;
```

**Reading is FREE** (gas only, no LINK required).

---

## Implementation Checklist

### Phase 1: Optimize Current Contract
- [ ] Evaluate removing `firstAuthor` storage (breaks tipping)
- [ ] Consider IPFS-based content storage
- [ ] Reduce gas from ~100k to ~25k

### Phase 2: Multi-Chain Deployment
- [ ] Deploy on Gnosis (primary)
- [ ] Consider Avalanche (larger ecosystem)
- [ ] Avoid L2s with blob expiry

### Phase 3: Cross-Chain Bridging
- [ ] Implement intent-based treasury
- [ ] Add optimistic proof with bonds
- [ ] Set 5% keeper fee
- [ ] Use Chainlink for USD pricing

---

## Key Takeaways

1. **Stay on Gnosis** - Cheapest, permanent, free archive, stablecoin native

2. **Avoid L2s** - Blob data expires in 18 days, defeating permanence goal

3. **IPFS optimization** - Reduces gas by ~85% ($0.002 per post)

4. **Intent-based bridging** - Zero infra, economic security via bonds

5. **Chainlink is free** - Reading price feeds costs only gas

---

## References

- [EIP-4844 Blob Expiry](https://hacken.io/discover/eip-4844-explained/)
- [Chainlink Price Feeds](https://docs.chain.link/data-feeds/price-feeds/addresses)
- [Gnosis RPC Providers](https://docs.gnosischain.com/tools/RPC%20Providers/)
- [ERC-7683 Cross-Chain Intents](https://www.erc7683.org/)
- [Across Protocol Security](https://blog.uma.xyz/articles/case-study-how-uma-secures-across-protocol)
