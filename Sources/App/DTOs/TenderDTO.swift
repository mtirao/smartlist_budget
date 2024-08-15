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
    let user_id: String?
    
    func toModel() -> Tender {
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
        
        if let userid = self.user_id {
            model.userId = userid
        }
        
        return model
    }
}
