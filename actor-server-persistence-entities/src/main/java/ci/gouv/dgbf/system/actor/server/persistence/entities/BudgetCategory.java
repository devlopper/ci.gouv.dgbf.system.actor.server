package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Cacheable;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=BudgetCategory.TABLE_NAME)
/* Performance Tuning */
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_ONLY)
@org.hibernate.annotations.Immutable
public class BudgetCategory extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public BudgetCategory setIdentifier(String identifier) {
		return (BudgetCategory) super.setIdentifier(identifier);
	}
	
	@Override
	public BudgetCategory setCode(String code) {
		return (BudgetCategory) super.setCode(code);
	}
	
	@Override
	public BudgetCategory setName(String name) {
		return (BudgetCategory) super.setName(name);
	}
	
	@Override
	public String toString() {
		return code+" "+name;
	}
	
	public static final String TABLE_NAME = "VM_APP_CAT_BUDGET";
	
	public static final String CODE_GENERAL = "01";
	public static final String CODE_EPN = "13";
	
	public static final String LABEL = "Catégorie budget";
}