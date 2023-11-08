package com.hunteryavitz.blockchainapi.services;

import com.hunteryavitz.blockchainapi.entities.Block;
import com.hunteryavitz.blockchainapi.entities.Transaction;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

/**
 * Unit tests for the BlockchainService class.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@TestPropertySource(locations = "classpath:application-test.properties")
public class BlockchainServiceTests {

    /**
     * The BlockchainService used to make requests to the API.
     */
    @Mock
    BlockchainService blockchainService;

    /**
     * Tests the createInitialBlockchain method.
     */
    @Test
    public void testCreateInitialBlockchain() {

        blockchainService = new BlockchainService();
        blockchainService.createInitialBlockchain();

        assert blockchainService.getBlockchain().length == 100;
        assert blockchainService.getBlockchain()[0].getIndex() == 0;
        assert blockchainService.getBlockchain()[0].getData().equals("Genesis Block");
    }

    /**
     * Tests the addBlockToBlockchain method.
     */
    @Test
    public void testAddBlockToBlockchain() {

        blockchainService = new BlockchainService();
        blockchainService.createInitialBlockchain();
        blockchainService.addBlockToBlockchain();

        assert blockchainService.getBlockchain().length == 100;
        assert blockchainService.getBlockchain()[1].getIndex() == 1;
        assert blockchainService.getBlockchain()[1].getData().equals("Block 1");
    }

    /**
     * Tests the getNextBlockIndexFromBlockchain method.
     */
    @Test
    public void testAddBlockToBlockchainFromTransaction() {

        blockchainService = new BlockchainService();
        blockchainService.createInitialBlockchain();

        Transaction[] transactionPool = new Transaction[1];
        Transaction transaction = new Transaction(1, "right_now", "your mom", "CREATED");
        transactionPool[0] = transaction;

        blockchainService.addBlockToBlockchain(transactionPool);

        String transactionFromBlockchain = blockchainService.getBlockchain()[1].getData();

        assert blockchainService.getBlockchain()[1].getIndex() == 1;
        assert transactionFromBlockchain.contains("your mom");
    }
}
//    Block[] mockBlockchain = {};
//    when(blockchainService.getBlockchain()).thenReturn(mockBlockchain);
//        ResponseEntity<Block[]> response = blockchainController.getBlockchain();
