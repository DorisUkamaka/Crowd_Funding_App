// Commit 1: Basic test setup with campaign creation
import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v0.14.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

Clarinet.test({
    name: "Ensure that users can create a campaign",
    async fn(chain: Chain, accounts: Map<string, Account>)
    {
        const deployer = accounts.get("deployer")!;
        const user1 = accounts.get("wallet_1")!;

        let block = chain.mineBlock([
            Tx.contractCall("crowdfunding", "create-campaign",
                [types.uint(1000000), types.uint(100)], // 1M STX goal, 100 blocks deadline
                user1.address
            )
        ]);

        assertEquals(block.receipts.length, 1);
        assertEquals(block.height, 2);
        assertEquals(block.receipts[0].result, '(ok u0)'); // First campaign should have ID 0
    },
});

Clarinet.test({
    name: "Ensure that campaign details can be retrieved",
    async fn(chain: Chain, accounts: Map<string, Account>)
    {
        const user1 = accounts.get("wallet_1")!;

        let block = chain.mineBlock([
            Tx.contractCall("crowdfunding", "create-campaign",
                [types.uint(1000000), types.uint(100)],
                user1.address
            )
        ]);

        block = chain.mineBlock([
            Tx.contractCall("crowdfunding", "get-campaign-details",
                [types.uint(0)],
                user1.address
            )
        ]);

        assertEquals(block.receipts.length, 1);
    },
});

Clarinet.test({
    name: "Ensure that users can contribute to a campaign",
    async fn(chain: Chain, accounts: Map<string, Account>)
    {
        const deployer = accounts.get("deployer")!;
        const user1 = accounts.get("wallet_1")!;
        const user2 = accounts.get("wallet_2")!;

        let block = chain.mineBlock([
            Tx.contractCall("crowdfunding", "create-campaign",
                [types.uint(1000000), types.uint(100)],
                user1.address
            ),
            Tx.contractCall("crowdfunding", "contribute",
                [types.uint(0), types.uint(500000)], // Contributing to campaign 0
                user2.address
            )
        ]);

        assertEquals(block.receipts.length, 2);
        assertEquals(block.receipts[1].result, '(ok true)');
    },
});

Clarinet.test({
    name: "Ensure campaign progress can be tracked",
    async fn(chain: Chain, accounts: Map<string, Account>)
    {
        const user1 = accounts.get("wallet_1")!;
        const user2 = accounts.get("wallet_2")!;

        let block = chain.mineBlock([
            Tx.contractCall("crowdfunding", "create-campaign",
                [types.uint(1000000), types.uint(100)],
                user1.address
            ),
            Tx.contractCall("crowdfunding", "contribute",
                [types.uint(0), types.uint(500000)],
                user2.address
            ),
            Tx.contractCall("crowdfunding", "get-campaign-progress",
                [types.uint(0)],
                user1.address
            )
        ]);

        assertEquals(block.receipts.length, 3);
    },
});

Clarinet.test({
    name: "Ensure that campaign owner can claim funds after successful campaign",
    async fn(chain: Chain, accounts: Map<string, Account>)
    {
        const user1 = accounts.get("wallet_1")!;
        const user2 = accounts.get("wallet_2")!;

        // Create campaign and contribute
        let block = chain.mineBlock([
            Tx.contractCall("crowdfunding", "create-campaign",
                [types.uint(1000000), types.uint(10)],
                user1.address
            ),
            Tx.contractCall("crowdfunding", "contribute",
                [types.uint(0), types.uint(1000000)],
                user2.address
            )
        ]);

        // Mine blocks to reach deadline
        chain.mineEmptyBlockUntil(12);

        // Try to claim funds
        block = chain.mineBlock([
            Tx.contractCall("crowdfunding", "claim-funds",
                [types.uint(0)],
                user1.address
            )
        ]);

        assertEquals(block.receipts[0].result, '(ok true)');
    },
});

Clarinet.test({
    name: "Ensure that contributors can get refund for failed campaign",
    async fn(chain: Chain, accounts: Map<string, Account>)
    {
        const user1 = accounts.get("wallet_1")!;
        const user2 = accounts.get("wallet_2")!;

        // Create campaign and contribute less than goal
        let block = chain.mineBlock([
            Tx.contractCall("crowdfunding", "create-campaign",
                [types.uint(1000000), types.uint(10)],
                user1.address
            ),
            Tx.contractCall("crowdfunding", "contribute",
                [types.uint(0), types.uint(500000)],
                user2.address
            )
        ]);

        // Mine blocks to reach deadline
        chain.mineEmptyBlockUntil(12);

        // Try to get refund
        block = chain.mineBlock([
            Tx.contractCall("crowdfunding", "refund",
                [types.uint(0)],
                user2.address
            )
        ]);

        assertEquals(block.receipts[0].result, '(ok true)');
    },
});

Clarinet.test({
    name: "Ensure that invalid operations are rejected",
    async fn(chain: Chain, accounts: Map<string, Account>)
    {
        const user1 = accounts.get("wallet_1")!;
        const user2 = accounts.get("wallet_2")!;

        // Try to create campaign with invalid goal
        let block = chain.mineBlock([
            Tx.contractCall("crowdfunding", "create-campaign",
                [types.uint(0), types.uint(100)],
                user1.address
            )
        ]);
        assertEquals(block.receipts[0].result, '(err u103)'); // err-invalid-amount

        // Try to contribute to non-existent campaign
        block = chain.mineBlock([
            Tx.contractCall("crowdfunding", "contribute",
                [types.uint(99), types.uint(1000)],
                user2.address
            )
        ]);
        assertEquals(block.receipts[0].result, '(err u101)'); // err-not-found

        // Create valid campaign
        block = chain.mineBlock([
            Tx.contractCall("crowdfunding", "create-campaign",
                [types.uint(1000000), types.uint(10)],
                user1.address
            )
        ]);

        // Try to claim before deadline
        block = chain.mineBlock([
            Tx.contractCall("crowdfunding", "claim-funds",
                [types.uint(0)],
                user1.address
            )
        ]);
        assertEquals(block.receipts[0].result, '(err u104)'); // err-deadline-passed
    },
});

Clarinet.test({
    name: "Ensure proper error handling for double claims and refunds",
    async fn(chain: Chain, accounts: Map<string, Account>)
    {
        const user1 = accounts.get("wallet_1")!;
        const user2 = accounts.get("wallet_2")!;

        // Create and fund successful campaign
        let block = chain.mineBlock([
            Tx.contractCall("crowdfunding", "create-campaign",
                [types.uint(1000000), types.uint(10)],
                user1.address
            ),
            Tx.contractCall("crowdfunding", "contribute",
                [types.uint(0), types.uint(1000000)],
                user2.address
            )
        ]);

        chain.mineEmptyBlockUntil(12);

        // Claim funds
        block = chain.mineBlock([
            Tx.contractCall("crowdfunding", "claim-funds",
                [types.uint(0)],
                user1.address
            )
        ]);

        // Try to claim again
        block = chain.mineBlock([
            Tx.contractCall("crowdfunding", "claim-funds",
                [types.uint(0)],
                user1.address
            )
        ]);
        assertEquals(block.receipts[0].result, '(err u106)'); // err-already-claimed
    },
});