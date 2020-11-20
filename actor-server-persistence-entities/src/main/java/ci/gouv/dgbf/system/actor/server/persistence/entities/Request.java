package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Collection;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=Request.TABLE_NAME)
public class Request extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_TYPE) @NotNull private RequestType type;
	@ManyToOne @JoinColumn(name = COLUMN_ACTOR) @NotNull private Actor actor;
	@Column(name = COLUMN_COMMENT) private String comment;
	@Column(name = COLUMN_CREATION_DATE) @NotNull private LocalDateTime creationDate;
	@Column(name = COLUMN_PROCESSING_DATE) private LocalDateTime processingDate;
	
	@ManyToOne @JoinColumn(name = COLUMN_ADMINISTRATIVE_UNIT) private AdministrativeUnit administrativeUnit;
	@Column(name = COLUMN_ADMINISTRATIVE_FUNCTION) private String administrativeFunction;
	@ManyToOne @JoinColumn(name = COLUMN_SECTION) private Section section;
	@ManyToOne @JoinColumn(name = COLUMN_BUDGET_SPECIALIZATION_UNIT) private BudgetSpecializationUnit budgetSpecializationUnit;		
	/*
	private String certificatReference;
	private String certificatSignatory;
	private LocalDateTime certificatSignatureDate;
	*/
	@Transient private Collection<Function> functions;
	@Transient private Collection<String> functionsAsStrings;
	@Transient private String typeAsString,actorAsString,actorCode,actorNames,creationDateAsString,processingDateAsString;
	
	@Override
	public Request setIdentifier(String identifier) {
		return (Request) super.setIdentifier(identifier);
	}
	
	public static final String FIELD_TYPE = "type";
	public static final String FIELD_TYPE_AS_STRING = "typeAsString";
	public static final String FIELD_ACTOR = "actor";
	public static final String FIELD_ACTOR_AS_STRING = "actorAsString";
	public static final String FIELD_ACTOR_CODE = "actorCode";
	public static final String FIELD_ACTOR_NAMES = "actorNames";
	public static final String FIELD_COMMENT = "comment";
	public static final String FIELD_CREATION_DATE = "creationDate";
	public static final String FIELD_CREATION_DATE_AS_STRING = "creationDateAsString";
	public static final String FIELD_PROCESSING_DATE = "processingDate";
	public static final String FIELD_PROCESSING_DATE_AS_STRING = "processingDateAsString";
	public static final String FIELD_FUNCTIONS = "functions";
	public static final String FIELD_FUNCTIONS_AS_STRINGS = "functionsAsStrings";
	
	public static final String FIELD_ADMINISTRATIVE_UNIT = "administrativeUnit";
	public static final String FIELD_ADMINISTRATIVE_FUNCTION = "administrativeFunction";
	public static final String FIELD_SECTION = "section";
	public static final String FIELD_BUDGET_SPECIALIZATION_UNIT = "budgetSpecializationUnit";
	
	public static final String TABLE_NAME = "DM_DEMANDE";
	
	public static final String COLUMN_TYPE = "TYPE";
	public static final String COLUMN_ACTOR = "ACTEUR";
	public static final String COLUMN_CREATION_DATE = "DATE_CREATION";
	public static final String COLUMN_PROCESSING_DATE = "DATE_TRAITEMENT";
	public static final String COLUMN_COMMENT = "COMMENTAIRE";
	
	public static final String COLUMN_ADMINISTRATIVE_UNIT = "UNITE_ADMINISTRATIVE";
	public static final String COLUMN_ADMINISTRATIVE_FUNCTION = "FONCTION_ADMINISTRATIVE";
	public static final String COLUMN_SECTION = "SECTION";
	public static final String COLUMN_BUDGET_SPECIALIZATION_UNIT = "USB";
}