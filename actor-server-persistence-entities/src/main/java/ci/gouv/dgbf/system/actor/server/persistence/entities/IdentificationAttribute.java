package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;

import javax.persistence.Cacheable;
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
@Entity @Table(name=IdentificationAttribute.TABLE_NAME)
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_WRITE)
public class IdentificationAttribute extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Column(name = COLUMN_REQUIRED) private Boolean required;
	
	@Transient private String requiredAsString;
	@Transient private Integer orderNumber;
	
	@Override
	public IdentificationAttribute setIdentifier(String identifier) {
		return (IdentificationAttribute) super.setIdentifier(identifier);
	}
	
	@Override
	public IdentificationAttribute setCode(String code) {
		return (IdentificationAttribute) super.setCode(code);
	}
	
	@Override
	public IdentificationAttribute setName(String name) {
		return (IdentificationAttribute) super.setName(name);
	}
	
	public static final String FIELD_REQUIRED = "required";
	public static final String FIELD_REQUIRED_AS_STRING = "requiredAsString";
	public static final String FIELD_ORDER_NUMBER = "orderNumber";
	
	public static final String TABLE_NAME = "ID_ATTRIBUT";	
	
	public static final String COLUMN_REQUIRED = "OBLIGATOIRE";
	
	/**/
	
	public static final String CODE_REGISTRATION_NUMBER = "MATRICULE";
	public static final String CODE_GROUP = "GROUPE";
	public static final String CODE_POSTAL_BOX_ADDRESS = "ADRESSE_BOITE_POSTALE";
	public static final String CODE_CIVILITY = "CIVILITE";
	public static final String CODE_FIRST_NAME = "NOM";
	public static final String CODE_LAST_NAMES = "PRENOMS";
	public static final String CODE_ELECTRONIC_MAIL_ADDRESS = "EMAIL";
	public static final String CODE_MOBILE_PHONE_NUMBER = "NUMERO_TELEPHONE_MOBILE";
	public static final String CODE_OFFICE_PHONE_NUMBER = "NUMERO_TELEPHONE_BUREAU";
	public static final String CODE_OFFICE_PHONE_EXTENSION = "POSTE_TELEPHONE_BUREAU";
	public static final String CODE_PHOTO = "PHOTO";
	public static final String CODE_SIGNATURE = "SIGNATURE";
	
	public static final String CODE_ACT_OF_APPOINTMENT = "ACTE_NOMINATION";
	public static final String CODE_ACT_OF_APPOINTMENT_REFERENCE = "REFERENCE_ACTE_NOMINATION";
	public static final String CODE_ACT_OF_APPOINTMENT_SIGNATORY = "SIGNATAIRE_ACTE_NOMINATION";
	public static final String CODE_ACT_OF_APPOINTMENT_SIGNATURE_DATE = "DATE_SIGNATURE_ACTE_NOMINATION";
	
	public static final String CODE_ADMINISTRATIVE_FUNCTION = "FONCTION_ADMINISTRATIVE";
	public static final String CODE_ADMINISTRATIVE_UNIT = "UNITE_ADMINISTRATIVE";
	public static final String CODE_SECTION = "SECTION";
	public static final String CODE_BUDGET_SPECIALIZATION_UNIT = "USB";
	public static final String CODE_CREDIT_MANAGER_HOLDER = "GESTIONNAIRE_CREDIT";
	public static final String CODE_AUTHORIZING_OFFICER_HOLDER = "ORDONNATEUR_DELEGUE";
	public static final String CODE_FINANCIAL_CONTROLLER_HOLDER = "CONTROLEUR_FINANCIER";
	public static final String CODE_ACCOUNTING_HOLDER = "COMPTABLE";
	public static final String CODE_COMMENT = "COMMENTAIRE";
	public static final String CODE_BUDGETARIES_FUNCTIONS = "FONCTIONS_BUDGETAIRES";
	public static final String CODE_BUDGETARIES_SCOPE_FUNCTIONS = "POSTES_BUDGETAIRES";
	
	public static final String CODE_BUDGETARY_EXERCICE = "EXERCICE_BUDGETAIRE";
	public static final String CODE_SIGNED_REQUEST_SHEET = "FICHE_DEMANDE_SIGNEE";
	
	public static final Collection<String> CODES_FIELDS_NAMES = new ArrayList<>();
	static {
		Collection<String> names = FieldHelper.getNames(FieldHelper.filter(ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationAttribute.class, "^CODE_", Modifier.STATIC));
		if(CollectionHelper.isNotEmpty(names))
			CODES_FIELDS_NAMES.addAll(names);
	}
}