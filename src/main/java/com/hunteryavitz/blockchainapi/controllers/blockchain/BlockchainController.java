package com.hunteryavitz.blockchainapi.controllers.blockchain;

import com.hunteryavitz.blockchainapi.entities.Block;
import com.hunteryavitz.blockchainapi.services.BlockchainService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * The BlockchainController class is the controller for the blockchain endpoints.
 */
@RestController
@RequestMapping("/api/v1/blockchain")
public class BlockchainController {

    /**
     * The blockchainService is the service for the blockchain endpoints.
     */
    private final BlockchainService blockchainService;

    /**
     * The constructor for the BlockchainController class.
     * @param blockchainService The service for the blockchain endpoints.
     */
    public BlockchainController(BlockchainService blockchainService) {
        this.blockchainService = blockchainService;
        if (blockchainService.getBlockchain() == null) {
            blockchainService.createInitialBlockchain();
        }
    }

    /**
     * The addBlockToBlockchain method is the endpoint for adding a block to the blockchain.
     * @return A ResponseEntity containing a boolean indicating whether the block was added to the blockchain.
     */
    @GetMapping("/verifyBlockchain")
    public ResponseEntity<Boolean> verifyBlockchain() {
        if (blockchainService.verifyBlockchain()) {
            return ResponseEntity.ok(true);
        }
        return ResponseEntity.ok(false);
    }

    /**
     * The getBlockchain method is the endpoint for getting the blockchain.
     * @return A ResponseEntity containing the blockchain.
     */
    @GetMapping("/getBlockchain")
    public ResponseEntity<Block[]> getBlockchain() {
        Block[] blockchain = blockchainService.getBlockchain();
        return ResponseEntity.ok(blockchain);
    }
}
