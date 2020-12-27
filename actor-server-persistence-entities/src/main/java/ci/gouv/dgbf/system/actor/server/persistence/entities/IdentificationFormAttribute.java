package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=IdentificationFormAttribute.TABLE_NAME)
public class IdentificationFormAttribute extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_FORM) private IdentificationForm form;	
	@ManyToOne @JoinColumn(name = COLUMN_ATTRIBUTE) private IdentificationAttribute attribute;
	@Column(name = COLUMN_NAME) private String name;
	@Column(name = COLUMN_ORDER_NUMBER) private Integer orderNumber;
	@Column(name = COLUMN_REQUIRED) private Boolean required;
	
	@Transient private String formAsString;
	@Transient private String attributeAsString;
	@Transient private String requiredAsString;
	
	@Override
	public IdentificationFormAttribute setIdentifier(String identifier) {
		return (IdentificationFormAttribute) super.setIdentifier(identifier);
	}
	
	public static final String FIELD_FORM = "form";
	public static final String FIELD_FORM_AS_STRING = "formAsString";
	public static final String FIELD_ATTRIBUTE = "attribute";
	public static final String FIELD_ATTRIBUTE_AS_STRING = "attributeAsString";
	public static final String FIELD_ORDER_NUMBER = "orderNumber";
	public static final String FIELD_NAME = "name";
	public static final String FIELD_REQUIRED = "required";
	public static final String FIELD_REQUIRED_AS_STRING = "requiredAsString";
	
	public static final String TABLE_NAME = "ID_CHAMP";
	
	public static final String COLUMN_FORM = "FORMULAIRE";
	public static final String COLUMN_ATTRIBUTE = "ATTRIBUT";
	public static final String COLUMN_NAME = "LIBELLE";
	public static final String COLUMN_ORDER_NUMBER = "NUMERO_ORDRE";
	public static final String COLUMN_REQUIRED = "OBLIGATOIRE";
	
	public static final String REQUIRED_NULL_VALUE_NAME = "Hérité";
}