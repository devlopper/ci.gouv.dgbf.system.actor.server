package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.cyk.utility.persistence.server.hibernate.entity.AbstractAuditIdentifiedByStringAndCodableAndNamable;
import org.hibernate.annotations.Immutable;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@AttributeOverrides(value= {
		@AttributeOverride(name = ScopeFunctionAudit.FIELD_IDENTIFIER,column = @Column(name=ScopeFunction.COLUMN_IDENTIFIER))
		,@AttributeOverride(name = ScopeFunctionAudit.FIELD_CODE,column = @Column(name=ScopeFunction.COLUMN_CODE))
		,@AttributeOverride(name = ScopeFunctionAudit.FIELD_NAME,column = @Column(name=ScopeFunction.COLUMN_NAME))
		,@AttributeOverride(name = ScopeFunctionAudit.FIELD___AUDIT_WHO__,column = @Column(name=ScopeFunction.COLUMN___AUDIT_WHO__))
		,@AttributeOverride(name = ScopeFunctionAudit.FIELD___AUDIT_WHAT__,column = @Column(name=ScopeFunction.COLUMN___AUDIT_WHAT__))
		,@AttributeOverride(name = ScopeFunctionAudit.FIELD___AUDIT_WHEN__,column = @Column(name=ScopeFunction.COLUMN___AUDIT_WHEN__))
		,@AttributeOverride(name = ScopeFunctionAudit.FIELD___AUDIT_FUNCTIONALITY__,column = @Column(name=ScopeFunction.COLUMN___AUDIT_FUNCTIONALITY__))
})
@Entity @Table(name = ScopeFunctionAudit.TABLE_NAME) @Getter @Setter @Accessors(chain=true) @Immutable
public class ScopeFunctionAudit extends AbstractAuditIdentifiedByStringAndCodableAndNamable implements Serializable {

	public static final String TABLE_NAME = ScopeFunction.AUDIT_TABLE_NAME;
}