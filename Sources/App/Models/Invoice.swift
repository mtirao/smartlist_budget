//
//  Invoice.swift
//
//
//  Created by Marcos Tirao on 29/07/2024.
//

import Fluent
import Foundation

final class Invoice: Model, @unchecked Sendable {
    static let schema = "invoices"
    
    @ID(key: .id)
    var id: UUID?

    @Field(key: "amount")
    var amount: Double
    
    @Field(key: "budget")
    var budget: UUID
    
    @Field(key: "date")
    var date: Date
    
    @Field(key: "name")
    var name: String
    
    @Field(key: "user_id")
    var userId: String

    init() { }

    init(id: UUID? = nil, amount: Double, budget: UUID, date: Date, name: String, userId: String) {
        self.id = id
        self.amount = amount
        self.budget = budget
        self.date = date
        self.name = name
        self.userId = userId
    }
    
    func toDTO() -> InvoiceDTO {
        .init(
            id: self.id,
            name: self.$name.value,
            amount: self.$amount.value, 
            budget: self.$budget.value,
            date: self.$date.value,
            user_id: self.$userId.value
        )
    }
}
