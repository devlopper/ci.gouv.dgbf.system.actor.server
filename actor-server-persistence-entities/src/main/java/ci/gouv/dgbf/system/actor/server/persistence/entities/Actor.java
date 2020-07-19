package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

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
@AttributeOverrides(value= {
		@AttributeOverride(name = Actor.FIELD_CODE,column = @Column(name="NOM_UTILISATEUR"))
})
public class Actor extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringImpl implements Identity.Interface,Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_IDENTITY) private Identity identity;
	@Column(name = COLUMN_CREATION_DATE) private LocalDateTime creationDate;
	
	@Transient private String firstName;
	@Transient private String lastNames;
	@Transient private String electronicMailAddress;	
	@Transient private String names;
	@Transient private Collection<Function> functions;
	@Transient private Collection<Privilege> privileges;
	@Transient private Collection<Scope> scopes;
	@Transient private String username;
	@Transient private String password;
	@Transient private Boolean keycloakUserCreatable;
	
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
	
	public Actor setIdentityFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setIdentity(null);
		else
			setIdentity(EntityFinder.getInstance().find(Identity.class, identifier));
		return this;
	}
	
	public static final String FIELD_IDENTITY = "identity";
	public static final String FIELD_FIRST_NAME = "firstName";
	public static final String FIELD_LAST_NAMES = "lastNames";
	public static final String FIELD_ELECTRONIC_MAIL_ADDRESS = "electronicMailAddress";
	public static final String FIELD_NAMES = "names";
	public static final String FIELD_CREATION_DATE = "creationDate";
	
	public static final String TABLE_NAME = "ACTEUR";
	
	public static final String COLUMN_CREATION_DATE = "DATE_CREATION";
	public static final String COLUMN_IDENTITY = "IDENTITE";
}