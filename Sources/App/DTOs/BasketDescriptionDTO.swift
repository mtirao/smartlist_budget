//
//  BasketDescriptionDTO.swift
//
//
//  Created by Marcos Tirao on 15/08/2024.
//

import Foundation
import Fluent
import Vapor

struct BasketDescriptionDTO: Content {

    var id: UUID?
    var itemId: UUID?
    var basketId: UUID?
    var price: Double?
    var lon: Double?
    var lat: Double?
    
    var name: String?
    var sku: String?
    var category: String?
    
    init(id: UUID? = nil, itemId: UUID? = nil, basketId: UUID? = nil, price: Double? = nil, lon: Double? = nil, lat: Double? = nil, name: String? = nil, sku: String? = nil, category: String? = nil) {
        self.id = id
        self.itemId = itemId
        self.basketId = basketId
        self.price = price
        self.lon = lon
        self.lat = lat
        self.name = name
        self.sku = sku
        self.category = category
    }
    
    func toModel(userId: String) -> BasketDesscription {
        let model = BasketDesscription()
        
        model.id = self.id
        
        if let itemId = self.itemId {
            model.$item.id = itemId
        }
        
        if let basketId = self.basketId {
            model.basketId = basketId
        }
        
        if let price = self.price {
            model.price = price
        }
        
        if let lon = self.lon {
            model.lon = lon
        }
        
        if let lat = self.lat {
            model.lat = lat
        }
        
        model.userId = userId

        return model
    }
}
