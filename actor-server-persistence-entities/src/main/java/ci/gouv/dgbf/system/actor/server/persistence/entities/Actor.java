package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringImpl;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Actor.TABLE_NAME)
public class Actor extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Column(name = COLUMN_FIRST_NAME) @NotNull private String firstName;
	@Column(name = COLUMN_LAST_NAMES) @NotNull private String lastNames;
	@Column(name = COLUMN_ELECTRONIC_MAIL_ADDRESS) @NotNull private String electronicMailAddress;
	
	@Transient private Collection<Function> functions;
	@Transient private String username;
	@Transient private String password;
	
	@Override
	public Actor setIdentifier(String identifier) {
		return (Actor) super.setIdentifier(identifier);
	}
	
	@Override
	public Actor setCode(String code) {
		return (Actor) super.setCode(code);
	}
	
	public Collection<Function> getFunctions(Boolean injectIfNull) {
		if(functions == null && Boolean.TRUE.equals(injectIfNull))
			functions = new ArrayList<>();
		return functions;
	}
	
	public Actor addFunctionsByIdentifiers(Collection<String> identifiers) {
		if(CollectionHelper.isEmpty(identifiers))
			return this;
		identifiers.forEach(identifier -> {
			if(StringHelper.isNotBlank(identifier)) {
				Function function = EntityFinder.getInstance().find(Function.class, identifier);
				if(function != null)
					getFunctions(Boolean.TRUE).add(function);
			}				
		});
		return this;
	}
	
	public Actor addFunctionsByIdentifiers(String...identifiers) {
		if(ArrayHelper.isEmpty(identifiers))
			return this;
		return addFunctionsByIdentifiers(CollectionHelper.listOf(identifiers));
	}
	
	public static final String FIELD_FIRST_NAME = "firstName";
	public static final String FIELD_LAST_NAMES = "lastNames";
	public static final String FIELD_ELECTRONIC_MAIL_ADDRESS = "electronicMailAddress";
	
	public static final String TABLE_NAME = "ACTEUR";
	
	public static final String COLUMN_FIRST_NAME = "NOM";
	public static final String COLUMN_LAST_NAMES = "PRENOMS";
	public static final String COLUMN_ELECTRONIC_MAIL_ADDRESS = "EMAIL";
}