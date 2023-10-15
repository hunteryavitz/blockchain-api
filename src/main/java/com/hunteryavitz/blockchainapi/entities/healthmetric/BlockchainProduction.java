package com.hunteryavitz.blockchainapi.entities.healthmetric;

import com.hunteryavitz.blockchainapi.utils.structures.SlidingWindow;
import lombok.Data;

@Data
public class BlockchainProduction {

    private SlidingWindow slidingWindow;

    public BlockchainProduction() {
        slidingWindow = new SlidingWindow(12);
    }

    public void updateProduction(int transactionCount, int blockCount) {
        slidingWindow.enqueueAndShift(transactionCount, blockCount);
    }


//    data: {
//        labels: ["1", "2", "3", "4", "5", "6", "7"],
//        datasets: [
//        {
//            label: "Transactions",
//                    data: [0, 0, 1, 2, 379, 482, 227],
//            backgroundColor: "rgba(54,73,93,.5)",
//                    borderColor: "#36495d",
//                borderWidth: 3,
//                tension: 1
//        },
//        {
//            label: "Blockchain Production",
//                    data: [0.166, 2.081, 3.003, 0.323, 954.792, 285.886, 51.514],
//            backgroundColor: "rgba(71, 183,132,.5)",
//                    borderColor: "#47b784",
//                borderWidth: 3,
//                tension: 1
//        }
//        ]
//    }

}
