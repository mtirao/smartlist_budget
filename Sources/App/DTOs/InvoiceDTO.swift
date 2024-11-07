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
    
    init(id: UUID? = nil, name: String? = nil, amount: Double? = nil, budget: UUID? = nil, date: Date? = nil ) {
        self.id = id
        self.name = name
        self.amount = amount
        self.budget = budget
        self.date = date
    }
    
    func toModel(userId: String) -> Invoice {
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
        
        model.userId = userId
        
        if let budget = self.budget {
            model.budget = budget
        }
        
        
        return model
    }
}

