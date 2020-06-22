package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Scope.TABLE_NAME)
public class Scope extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_TYPE) @NotNull private ScopeType type;
	
	@Override
	public Scope setIdentifier(String identifier) {
		return (Scope) super.setIdentifier(identifier);
	}
	
	@Override
	public Scope setCode(String code) {
		return (Scope) super.setCode(code);
	}
	
	@Override
	public Scope setName(String name) {
		return (Scope) super.setName(name);
	}
	
	public Scope setTypeFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setType(null);
		else
			setType(EntityFinder.getInstance().find(ScopeType.class, identifier));
		return this;
	}
	
	public static final String FIELD_TYPE = "type";
	
	public static final String COLUMN_TYPE = "type";
	
	public static final String TABLE_NAME = "DOMAINE";	
}