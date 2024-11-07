//
//  Tender.swift
//
//
//  Created by Marcos Tirao on 15/08/2024.
//

import Fluent
import Foundation

final class Item: Model, @unchecked Sendable {
    static let schema = "items"
    
    @ID(key: .id)
    var id: UUID?
    
    @Field(key: "name")
    var name: String
    
    @Field(key: "sku")
    var sku: String
    
    @Field(key: "category")
    var category: String
    
    @Field(key: "user_id")
    var userId: String

    init() { }

    init(id: UUID? = nil, name: String, sku: String, category: String, userId: String) {
        self.id = id
        self.name = name
        self.sku = sku
        self.category = category
        self.userId = userId
    }
    
    func toDTO() -> ItemDTO {
        .init(
            id: self.id,
            name: self.$name.value,
            sku: self.$sku.value,
            category: self.$category.value
        )
    }
}
