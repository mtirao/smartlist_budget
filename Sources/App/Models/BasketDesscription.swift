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
    
    @Field(key: "date")
    var date: Date?
    
    @Parent(key: "item_id")
    var item: Item
    
    @Field(key: "user_id")
    var userId: String
    
    @Field(key: "basket_id")
    var basketId: UUID
    
    @Field(key: "price")
    var price: Double
    
    @Field(key: "lon")
    var lon: Double?
    
    @Field(key: "lat")
    var lat: Double?
    
    init() { }

    init(id: UUID? = nil, item: Item, userId: String, basketId: UUID, price: Double, date: Date? = nil, lon: Double? = nil, lat: Double? = nil) {
        self.id = id
        self.item = item
        self.userId = userId
        self.basketId = basketId
        self.price = price
        self.lon = lon
        self.lat = lat
        self.date = date
    }
    
    func toDTO() -> BasketDescriptionDTO {
        return BasketDescriptionDTO(
            id: self.id,
            itemId: self.item.id,
            basketId:self.basketId,
            price: self.price,
            lon: self.lon,
            lat: self.lat,
            name: self.item.name,
            sku: self.item.sku,
            category: self.item.category)
    }
}
