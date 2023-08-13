package com.hunteryavitz.blockchainapi.controllers.block;

import com.hunteryavitz.blockchainapi.services.BlockchainService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * The BlockController class is the controller for the block endpoints.
 */
@RestController
@CrossOrigin
@RequestMapping("/api/v1/block")
public class BlockController {

    /**
     * The blockchainService field is the service for the blockchain.
     */
    private final BlockchainService blockchainService;

    /**
     * The BlockController constructor is responsible for initializing the
     * @param blockchainService The blockchain service.
     */
    public BlockController(BlockchainService blockchainService) {
        this.blockchainService = blockchainService;
        if (blockchainService.getBlockchain() == null) {
            blockchainService.createInitialBlockchain();
        }
    }

    /**
     * The addBlockToBlockchain method is responsible for adding a block to the blockchain.
     * @return A ResponseEntity containing a boolean indicating whether or not the block was added to the blockchain.
     */
    @PostMapping("/addBlockToBlockchain")
    public ResponseEntity<Boolean> addBlockToBlockchain() {

        try {
            blockchainService.addBlockToBlockchain();
        } catch (Exception e) {
            return ResponseEntity.ok(false);
        }
        return ResponseEntity.ok(true);
    }
}
