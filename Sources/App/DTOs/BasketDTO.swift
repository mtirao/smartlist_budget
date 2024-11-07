//
//  TenderDTO.swift
//
//
//  Created by Marcos Tirao on 15/08/2024.
//

import Foundation
import Fluent
import Vapor

struct ItemDTO: Content {

    let id: UUID?
    var name: String?
    var sku: String?
    var category: String?
    
    init( id: UUID? = nil, name: String? = nil, sku: String? = nil, category: String? = nil) {
        self.id = id
        self.name = name
        self.sku = sku
        self.category = category
    }
    
    func toModel(userId: String) -> Item {
        let model = Item()
        
        model.id = self.id
        if let name = self.name {
            model.name = name
        }
        
        if let sku = self.sku {
            model.sku = sku
        }
        
        if let category = self.category {
            model.category = category
        }
        
        model.userId = userId

        return model
    }
}
