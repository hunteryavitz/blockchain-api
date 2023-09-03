package com.hunteryavitz.blockchainapi.controllers;

import com.hunteryavitz.blockchainapi.entities.Block;
import com.hunteryavitz.blockchainapi.services.BlockchainService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * The BlockchainController class is the controller for the blockchain endpoints.
 */
@RestController
@CrossOrigin
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

    /**
     * The getBlockById method is the endpoint for getting a block by its id.
     * @param id The id of the block.
     * @return A ResponseEntity containing the block.
     */
    @GetMapping("/getBlockById")
    public ResponseEntity<Block> getBlockById(@RequestParam int id) {

        Block block = blockchainService.getBlockById(id);
        return ResponseEntity.ok(block);
    }
}
