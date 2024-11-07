//
//  BasketDesscription.swift
//
//
//  Created by Marcos Tirao on 15/08/2024.
//

import Fluent
import Foundation


final class BasketDesscription: Model, @unchecked Sendable {
    static let schema = "basket_descriptions"
    
    @ID(key: .id)
    var id: UUID?
    
    @Field(key: "item_id")
    var itemId: UUID
    
    @Field(key: "user_id")
    var userId: String
    
    @Field(key: "basket_id")
    var basketId: UUID
    
    @Field(key: "price")
    var price: Double
    
    @Field(key: "lon")
    var lon: Double
    
    @Field(key: "lat")
    var lat: Double
    
    init() { }

    init(id: UUID? = nil, itemId: UUID, userId: String, basketId: UUID, price: Double, lon: Double, lat: Double) {
        self.id = id
        self.itemId = itemId
        self.userId = userId
        self.basketId = basketId
        self.price = price
        self.lon = lon
        self.lat = lat
    }
    
    func toDTO() -> BasketDescriptionDTO {
        .init(
            id: self.id,
            itemId: self.$itemId.value,
            basketId: self.$basketId.value,
            price: self.$price.value,
            lon: self.$lon.value,
            lat: self.$lat.value)
        
    }
}
