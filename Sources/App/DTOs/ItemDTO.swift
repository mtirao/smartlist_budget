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
    var date: Date?
    var status: Status?
    
    init( id: UUID? = nil, date: Date? = nil, status: Status? = nil) {
        self.id = id
        self.date = date
        self.status = status
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
