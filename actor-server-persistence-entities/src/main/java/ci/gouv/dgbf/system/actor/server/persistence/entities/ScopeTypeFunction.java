package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ScopeTypeFunction.TABLE_NAME)
public class ScopeTypeFunction extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_SCOPE_TYPE) @NotNull private ScopeType scopeType;
	@ManyToOne @JoinColumn(name = COLUMN_FUNCTION) @NotNull private Function function;
	@Column(name = COLUMN_SCOPE_FUNCTION_DERIVABLE) private Boolean scopeFunctionDerivable;
	
	@Transient private String scopeTypeAsString;
	@Transient private String functionAsString;
	@Transient private String scopeFunctionDerivableAsString;
	
	@Override
	public ScopeTypeFunction setIdentifier(String identifier) {
		return (ScopeTypeFunction) super.setIdentifier(identifier);
	}
	
	public ScopeTypeFunction setScopeTypeFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setScopeType(null);
		else
			setScopeType(EntityFinder.getInstance().find(ScopeType.class, identifier));
		return this;
	}
	
	public ScopeTypeFunction setFunctionFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setFunction(null);
		else
			setFunction(EntityFinder.getInstance().find(Function.class, identifier));
		return this;
	}
	
	public static final String FIELD_SCOPE_TYPE = "scopeType";
	public static final String FIELD_SCOPE_TYPE_AS_STRING = "scopeTypeAsString";
	public static final String FIELD_FUNCTION = "function";
	public static final String FIELD_FUNCTION_AS_STRING = "functionAsString";
	public static final String FIELD_SCOPE_FUNCTION_DERIVABLE = "scopeFunctionDerivable";
	public static final String FIELD_SCOPE_FUNCTION_DERIVABLE_AS_STRING = "scopeFunctionDerivableAsString";
	
	public static final String TABLE_NAME = "TYPE_DOMAINE_FONCTION";
	
	public static final String COLUMN_SCOPE_TYPE = "TYPE_DOMAINE";
	public static final String COLUMN_FUNCTION = "FONCTION";
	public static final String COLUMN_SCOPE_FUNCTION_DERIVABLE = "POSTE_DERIVABLE";
}