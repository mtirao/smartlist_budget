//
//  TenderDTO.swift
//
//
//  Created by Marcos Tirao on 15/08/2024.
//

import Foundation
import Fluent
import Vapor



struct TenderDTO: Content {
    let id: UUID?
    let type: String?
    let number: String?
    let alias: String?
    
    init() {
        self.id = nil
        self.type = nil
        self.number = nil
        self.alias = nil
    }
    
    init(id: UUID? = nil, type: String? = nil, number: String? = nil, alias: String? = nil ) {
        self.id = id
        self.type = type
        self.number = number
        self.alias = alias
    }
    
    func toModel(userId: String) -> Tender {
        let model = Tender()
        
        model.id = self.id
        if let type = self.type {
            model.type = type
        }
        
        if let number = self.number {
            model.number = number
        }
        
        if let alias = self.alias {
            model.alias = alias
        }
        
        model.userId = userId
        
        return model
    }
}
