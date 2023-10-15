package com.hunteryavitz.blockchainapi.entities.healthmetric;

import lombok.Data;

@Data
public class HealthMetricProductionModel {

    private Data data;

    public HealthMetricProductionModel() {
        data = new Data();
    }

    public void setDataset1(int[] data) {
        this.data.setData1(data);
    }

    public void setDataset2(int[] data) {
        this.data.setData2(data);
    }

    class Data {
        private int[] dataset1;
        private int[] dataset2;

        public void setData1(int[] data) {
            this.dataset1 = data;
        }

        public void setData2(int[] data) {
            this.dataset2 = data;
        }
    }

    public String asJson() {
        return "{" +
                "\"data\": {" +
                "\"dataset1\": " + asJsonArray(data.dataset1) + "," +
                "\"dataset2\": " + asJsonArray(data.dataset2) +
                "}" +
                "}";
    }

    private String asJsonArray(int[] dataset1) {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        for (int i = 0; i < dataset1.length; i++) {
            sb.append(dataset1[i]);
            if (i != dataset1.length - 1) {
                sb.append(",");
            }
        }
        sb.append("]");
        return sb.toString();
    }
}
