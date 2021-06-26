package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.cyk.utility.persistence.server.hibernate.entity.AbstractAuditIdentifiedByString;
import org.hibernate.annotations.Immutable;

import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@AttributeOverrides(value= {
		@AttributeOverride(name = AssignmentsAudit.FIELD_IDENTIFIER,column = @Column(name=Assignments.COLUMN_IDENTIFIER))
		,@AttributeOverride(name = AssignmentsAudit.FIELD___AUDIT_WHO__,column = @Column(name=Assignments.COLUMN___AUDIT_WHO__))
		,@AttributeOverride(name = AssignmentsAudit.FIELD___AUDIT_WHAT__,column = @Column(name=Assignments.COLUMN___AUDIT_WHAT__))
		,@AttributeOverride(name = AssignmentsAudit.FIELD___AUDIT_WHEN__,column = @Column(name=Assignments.COLUMN___AUDIT_WHEN__))
		,@AttributeOverride(name = AssignmentsAudit.FIELD___AUDIT_FUNCTIONALITY__,column = @Column(name=Assignments.COLUMN___AUDIT_FUNCTIONALITY__))
})
@Entity @Table(name = AssignmentsAudit.TABLE_NAME) @Getter @Setter @Accessors(chain=true) @Immutable
public class AssignmentsAudit extends AbstractAuditIdentifiedByString implements Serializable {

	@Column(name = Assignments.COLUMN_CREDIT_MANAGER_HOLDER)
	private String creditManagerHolder;
	
	@Column(name = Assignments.COLUMN_AUTHORIZING_OFFICER_HOLDER)
	private String authorizingOfficerHolder;
	
	@Column(name = Assignments.COLUMN_FINANCIAL_CONTROLLER_HOLDER)
	private String financialControllerHolder;
	
	@Column(name = Assignments.COLUMN_ACCOUNTING_HOLDER)
	private String accountingHolder;
	
	public static final String TABLE_NAME = Assignments.AUDIT_TABLE_NAME;
}