package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Profile.TABLE_NAME)
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_WRITE)
public class Profile extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_TYPE) @NotNull 	
	private ProfileType type;
	@Transient private String typeAsString;
	@Column(name = COLUMN_ORDER_NUMBER) private Byte orderNumber;
	@Column(name = COLUMN_REQUESTABLE) private Boolean requestable;
	@Transient private String requestableAsString;
	
	@Transient private Collection<Privilege> privileges;
	@Transient private Collection<String> privilegesAsStrings;
	
	@Transient private Boolean used;
	@Transient private String usedAsString;
	@Transient private Integer numberOfActors;
	
	@Override
	public Profile setIdentifier(String identifier) {
		return (Profile) super.setIdentifier(identifier);
	}
	
	@Override
	public Profile setCode(String code) {
		return (Profile) super.setCode(code);
	}
	
	@Override
	public Profile setName(String name) {
		return (Profile) super.setName(name);
	}
	
	public Profile setTypeFromIdentifier(String identifier) {
		if(StringHelper.isBlank(identifier))
			setType(null);
		else
			setType(EntityFinder.getInstance().find(ProfileType.class, identifier));
		return this;
	}

	public Collection<Privilege> getPrivileges(Boolean injectIfNull) {
		if(privileges == null && Boolean.TRUE.equals(injectIfNull))
			privileges = new ArrayList<>();
		return privileges;
	}
	
	public Profile addPrivilegesByIdentifiers(Collection<String> identifiers) {
		if(CollectionHelper.isEmpty(identifiers))
			return this;
		identifiers.forEach(identifier -> {
			if(StringHelper.isNotBlank(identifier)) {
				Privilege privilege = EntityFinder.getInstance().find(Privilege.class, identifier);
				if(privilege != null)
					getPrivileges(Boolean.TRUE).add(privilege);
			}				
		});
		return this;
	}
	
	public Profile addPrivilegesByIdentifiers(String...identifiers) {
		if(ArrayHelper.isEmpty(identifiers))
			return this;
		return addPrivilegesByIdentifiers(CollectionHelper.listOf(identifiers));
	}
	
	public static final String FIELD_TYPE = "type";
	public static final String FIELD_TYPE_IDENTIFIER = "typeIdentifier";
	public static final String FIELD_TYPE_AS_STRING = "typeAsString";
	public static final String FIELD_ORDER_NUMBER = "orderNumber";
	public static final String FIELD_REQUESTABLE = "requestable";
	public static final String FIELD_PRIVILEGES = "privileges";
	public static final String FIELD_PRIVILEGES_AS_STRINGS = "privilegesAsStrings";
	public static final String FIELD_NUMBER_OF_ACTORS = "numberOfActors";
	public static final String FIELDS_REQUESTABLE_AND_REQUESTABLE_AS_STRING = "requestableAndRequestableAsString";
	
	public static final String TABLE_NAME = "PROFILE";
	
	public static final String COLUMN_TYPE = "TYPE";
	public static final String COLUMN_ORDER_NUMBER = "NUMERO_ORDRE";
	public static final String COLUMN_REQUESTABLE = "DEMANDABLE";
	
	public static final String CODE_UTILISATEUR = "UTILISATEUR";
	public static final String CODE_ADMINISTRATEUR = "ADMINISTRATEUR";
	public static final String CODE_TRAITEUR_DEMANDE_ENROLEMENT = "TRAITEUR_DEMANDE_ENROLEMENT";
	public static final String CODE_CHARGE_ETUDE_DAS = "CEDAS";
	public static final String CODE_RESPONSABLE_FONCTION_FINANCIERE_MINISTERE = "RFFIM";
	public static final String CODE_REQUERANT_CELIOPE = "REQUERANT_CELIOPE";
	
	public static final String LABEL = "Profile";
}