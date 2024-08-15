//
//  InvoiceDTO.swift
//
//
//  Created by Marcos Tirao on 29/07/2024.
//

import Foundation
import Fluent
import Vapor

struct InvoiceDTO: Content {
    let id: UUID?
    let name: String?
    let amount: Double?
    let budget: UUID?
    let date: Date?
    let user_id: String?
    
    func toModel() -> Invoice {
        let model = Invoice()
        
        model.id = self.id
        if let name = self.name {
            model.name = name
        }
        
        if let date {
            model.date = date
        }else {
            model.date = Date()
        }
        
        if let amount = self.amount {
            model.amount = amount
        }
        
        if let userid = self.user_id {
            model.userId = userid
        }
        
        if let budget = self.budget {
            model.budget = budget
        }
        
        
        return model
    }
}

