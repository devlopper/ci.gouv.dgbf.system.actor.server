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
@Entity @Table(name=BudgetaryFunction.TABLE_NAME)
@Cacheable
public class BudgetaryFunction extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public BudgetaryFunction setIdentifier(String identifier) {
		return (BudgetaryFunction) super.setIdentifier(identifier);
	}
	
	@Override
	public BudgetaryFunction setCode(String code) {
		return (BudgetaryFunction) super.setCode(code);
	}
	
	@Override
	public BudgetaryFunction setName(String name) {
		return (BudgetaryFunction) super.setName(name);
	}
	
	public static final String TABLE_NAME = "FONCTION_BUDGETAIRE";	
}