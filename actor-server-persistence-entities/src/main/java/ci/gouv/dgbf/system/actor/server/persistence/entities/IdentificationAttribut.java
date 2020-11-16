package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=IdentificationAttribut.TABLE_NAME)
public class IdentificationAttribut extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Column(name = COLUMN_REQUIRED) private Boolean required;
	
	@Transient private Integer orderNumber;
	
	@Override
	public IdentificationAttribut setIdentifier(String identifier) {
		return (IdentificationAttribut) super.setIdentifier(identifier);
	}
	
	public static final String FIELD_REQUIRED = "required";
	
	public static final String TABLE_NAME = "ID_ATTRIBUT";	
	
	public static final String COLUMN_REQUIRED = "OBLIGATOIRE";
	
	/**/
	
	public static final String CODE_CIVILITY = "CIVILITE";
	public static final String CODE_FIRST_NAME = "NOM";
	public static final String CODE_LAST_NAMES = "PRENOMS";
	public static final String CODE_ELECTRONIC_MAIL_ADDRESS = "EMAIL";
	public static final String CODE_MOBILE_PHONE_NUMBER = "NUMERO_TELEPHONE_MOBILE";
	public static final String CODE_OFFICE_PHONE_NUMBER = "NUMERO_TELEPHONE_BUREAU";
	
	public static final Collection<String> CODES_FIELDS_NAMES = new ArrayList<>();
	static {
		Collection<String> names = FieldHelper.getNames(FieldHelper.filter(ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationAttribut.class, "^CODE_", Modifier.STATIC));
		if(CollectionHelper.isNotEmpty(names))
			CODES_FIELDS_NAMES.addAll(names);
	}
}